## Email Providers

`gcourier` supports any mail provider that offers SMTP (which includes the vast majority of providers). We do not support SMTPS (implicit TLS, typically on port 465), but we do support STARTTLS (explicit TLS upgrade). If your provider offers multiple ports, you should generally choose **port 587**. If 587 is not available, try port 25 or 2525.

Most consumer email providers (like Gmail, Outlook, Yahoo) require you to generate an **app-specific password**. Others (like Mailgun or Amazon SES) will provide you with SMTP credentials.

The username you pass to `smtp.send` should be the SMTP credential provided by your email provider—not necessarily the email address you're sending from.

If you are using a provider not documented here and encounter issues, please feel free to open an issue, and we will try to provide support. We would also appreciate it if you update this document with any provider-specific idiosyncrasies you discover.

## Provider-Specific Instructions
- [Amazon SES](/providers/ses.html)
- [Gmail](/providers/gmail.html)
