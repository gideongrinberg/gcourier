package main

import (
	"bufio"
	"crypto/tls"
	"crypto/x509"
	"flag"
	"io"
	"log"
	"net"
	"strings"
	"time"
)

const (
	timeout = 5 * time.Minute
)

func main() {
	// Parse command-line arguments
	clientAddr := flag.String("listen", ":1025", "Address to listen for client connections")
	serverAddr := flag.String("server", "localhost:25", "Address of the SMTP server")
	verbose := flag.Bool("verbose", false, "Enable verbose logging")
	flag.Parse()

	certPool, err := x509.SystemCertPool()
	if err != nil {
		log.Fatalf("Error loading certificate and key: %v", err)
	}

	tlsConfig := &tls.Config{
		RootCAs: certPool,
	}

	if *verbose {
		log.SetFlags(log.Ldate | log.Ltime | log.Lmicroseconds | log.Lshortfile)
	} else {
		log.SetFlags(log.Ldate | log.Ltime)
	}

	listener, err := net.Listen("tcp", *clientAddr)
	if err != nil {
		log.Fatalf("Error listening: %v", err)
	}
	defer listener.Close()
	log.Printf("SMTP proxy listening on %s, forwarding to %s", *clientAddr, *serverAddr)

	for {
		clientConn, err := listener.Accept()
		if err != nil {
			log.Printf("Error accepting connection: %v", err)
			continue
		}
		go handleConnection(clientConn, *serverAddr, tlsConfig, *verbose)
	}
}

func handleConnection(clientConn net.Conn, serverAddr string, tlsConfig *tls.Config, verbose bool) {
	defer clientConn.Close()
	clientConn.SetDeadline(time.Now().Add(timeout))
	log.Printf("New client connection from %s", clientConn.RemoteAddr())
	serverConn, err := net.Dial("tcp", serverAddr)
	if err != nil {
		log.Printf("Error connecting to server: %v", err)
		clientConn.Write([]byte("421 4.3.0 Cannot connect to SMTP server\r\n"))
		return
	}
	defer serverConn.Close()
	serverConn.SetDeadline(time.Now().Add(timeout))

	// Set up readers and writers
	clientReader := bufio.NewReader(clientConn)
	serverReader := bufio.NewReader(serverConn)

	// Get server greeting
	greeting, err := readSMTPResponse(serverReader)
	if err != nil {
		log.Printf("Error reading server greeting: %v", err)
		clientConn.Write([]byte("421 4.3.0 Cannot get greeting from SMTP server\r\n"))
		return
	}
	if _, err := clientConn.Write([]byte(greeting)); err != nil {
		log.Printf("Error sending greeting to client: %v", err)
		return
	}

	if verbose {
		log.Printf("Server greeting: %s", strings.TrimSpace(greeting))
	}

	// Main proxy loop
	inDataPhase := false
	for {
		// Reset timeouts on each iteration
		clientConn.SetDeadline(time.Now().Add(timeout))
		serverConn.SetDeadline(time.Now().Add(timeout))

		// Read client command
		command, err := clientReader.ReadString('\n')
		if err != nil {
			if err != io.EOF {
				log.Printf("Error reading client command: %v", err)
			} else {
				log.Printf("Client closed connection")
			}
			return
		}

		cmdUpper := strings.ToUpper(strings.TrimSpace(command))
		if verbose {
			log.Printf("Client: %s", strings.TrimSpace(command))
		}

		// Handle DATA command specially - need to handle the data phase
		if inDataPhase {
			// In DATA phase, we forward everything until we see a line with just a dot
			if strings.TrimSpace(command) == "." {
				inDataPhase = false
				if verbose {
					log.Printf("End of DATA phase")
				}
			}
			// Forward the data to the server
			if _, err := serverConn.Write([]byte(command)); err != nil {
				log.Printf("Error forwarding DATA to server: %v", err)
				return
			}
			// If this is the end of DATA, get the server's response
			if !inDataPhase {
				response, err := readSMTPResponse(serverReader)
				if err != nil {
					log.Printf("Error reading server response to DATA: %v", err)
					return
				}
				if verbose {
					log.Printf("Server (DATA end): %s", strings.TrimSpace(response))
				}
				if _, err := clientConn.Write([]byte(response)); err != nil {
					log.Printf("Error forwarding DATA response to client: %v", err)
					return
				}
			}
			continue
		}

		// Check for beginning of DATA phase
		if strings.HasPrefix(cmdUpper, "DATA") {
			// Forward DATA command to server
			if _, err := serverConn.Write([]byte(command)); err != nil {
				log.Printf("Error forwarding DATA command to server: %v", err)
				return
			}
			// Get server's response to DATA command
			response, err := readSMTPResponse(serverReader)
			if err != nil {
				log.Printf("Error reading server response to DATA: %v", err)
				return
			}

			if verbose {
				log.Printf("Server (DATA cmd): %s", strings.TrimSpace(response))
			}

			// Check if server accepted DATA command (code 354)
			respCode := getResponseCode(response)
			if respCode == "354" {
				inDataPhase = true
				if verbose {
					log.Printf("Entering DATA phase")
				}
			}

			// Forward response to client
			if _, err := clientConn.Write([]byte(response)); err != nil {
				log.Printf("Error forwarding DATA response to client: %v", err)
				return
			}
			continue
		}

		// Handle QUIT command specially
		if strings.HasPrefix(cmdUpper, "QUIT") {
			if _, err := serverConn.Write([]byte(command)); err != nil {
				log.Printf("Error forwarding QUIT to server: %v", err)
				clientConn.Write([]byte("221 2.0.0 Bye\r\n"))
				return
			}

			response, err := readSMTPResponse(serverReader)
			if err != nil {
				log.Printf("Error reading server response to QUIT: %v", err)
				clientConn.Write([]byte("221 2.0.0 Bye\r\n"))
			} else {
				if verbose {
					log.Printf("Server (QUIT): %s", strings.TrimSpace(response))
				}
				clientConn.Write([]byte(response))
			}
			log.Printf("Client quit, closing connection")
			return
		}

		// Handle STARTTLS command
		if strings.HasPrefix(cmdUpper, "STARTTLS") {
			log.Printf("STARTTLS command received")

			// Forward STARTTLS to server
			if _, err := serverConn.Write([]byte(command)); err != nil {
				log.Printf("Error forwarding STARTTLS to server: %v", err)
				return
			}

			// Read server response
			response, err := readSMTPResponse(serverReader)
			if err != nil {
				log.Printf("Error reading STARTTLS response from server: %v", err)
				return
			}

			// Check if server agreed to STARTTLS
			responseCode := getResponseCode(response)
			if responseCode != "220" {
				log.Printf("Server did not agree to STARTTLS: %s", strings.TrimSpace(response))
				clientConn.Write([]byte(response))
				continue
			}

			log.Printf("Server agreed to STARTTLS")

			// Upgrade ONLY the server connection to TLS
			log.Printf("Upgrading server connection to TLS")
			tlsServerConn := tls.Client(serverConn, &tls.Config{
				InsecureSkipVerify: true, // Skip verification for simplicity
			})
			err = tlsServerConn.Handshake()
			if err != nil {
				log.Printf("TLS handshake with server failed: %v", err)
				clientConn.Write([]byte("421 4.7.0 TLS handshake with server failed\r\n"))
				return
			}
			serverConn = tlsServerConn
			serverReader = bufio.NewReader(serverConn)
			log.Printf("Server TLS handshake successful")

			// Send success response to client (as if STARTTLS succeeded)
			// We're not actually going to use TLS with the client
			if _, err := clientConn.Write([]byte("220 Ready for TLS transmission\r\n")); err != nil {
				log.Printf("Error sending STARTTLS success to client: %v", err)
				return
			}

			log.Printf("STARTTLS (server-side only) completed successfully")
			continue
		}

		// Forward command to server
		if _, err := serverConn.Write([]byte(command)); err != nil {
			log.Printf("Error forwarding command to server: %v", err)
			return
		}

		// Read server response and forward to client
		response, err := readSMTPResponse(serverReader)
		if err != nil {
			log.Printf("Error reading server response: %v", err)
			return
		}

		if verbose {
			log.Printf("Server: %s", strings.TrimSpace(response))
		}

		if _, err := clientConn.Write([]byte(response)); err != nil {
			log.Printf("Error forwarding response to client: %v", err)
			return
		}
	}
}

// readSMTPResponse reads a complete SMTP response, which may span multiple lines
func readSMTPResponse(reader *bufio.Reader) (string, error) {
	var fullResponse string

	for {
		line, err := reader.ReadString('\n')
		if err != nil {
			return fullResponse, err
		}

		fullResponse += line

		// Check if it's the last line of the response
		// SMTP response format:
		// - 3-digit code followed by space for single-line responses or the last line of multi-line responses
		// - 3-digit code followed by hyphen for continuation lines
		if len(line) >= 4 && line[3] == ' ' {
			break
		}
	}

	return fullResponse, nil
}

// getResponseCode extracts the SMTP response code from a response
func getResponseCode(response string) string {
	if len(response) >= 3 {
		return response[:3]
	}
	return ""
}
