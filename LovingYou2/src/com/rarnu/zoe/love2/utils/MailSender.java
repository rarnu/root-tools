package com.rarnu.zoe.love2.utils;

import java.util.Properties;

import javax.activation.DataHandler;
import javax.mail.Authenticator;
import javax.mail.Message;
import javax.mail.MessagingException;
import javax.mail.PasswordAuthentication;
import javax.mail.Session;
import javax.mail.Transport;
import javax.mail.internet.AddressException;
import javax.mail.internet.InternetAddress;
import javax.mail.internet.MimeMessage;
import javax.mail.util.ByteArrayDataSource;

import android.util.Log;

public class MailSender extends Authenticator {

	public static final String host = "smtp.gmail.com";

	private final String userName;
	private final String password;

	private Session session;

	public MailSender(String userName, String password) {
		this.userName = userName;
		this.password = password;

		initialize();
	}

	private void initialize() {
		Properties props = new Properties();
		props.setProperty("mail.transport.protocol", "smtp");
		props.setProperty("mail.smtp.host", host);
		props.setProperty("mail.smtp.socketFactory.class",
				"javax.net.ssl.SSLSocketFactory");
		props.setProperty("mail.smtp.socketFactory.fallback", "false");
		props.setProperty("mail.smtp.port", "465");
		props.setProperty("mail.smtp.socketFactory.port", "465");
		props.setProperty("mail.smtp.auth", "true");
		props.setProperty("mail.smtp.ssl", "true");

		session = Session.getDefaultInstance(props, this);
	}

	@Override
	protected PasswordAuthentication getPasswordAuthentication() {
		return new PasswordAuthentication(userName, password);
	}

	/**
	 * 发送Email
	 * 
	 * @param subject
	 *            标题
	 * @param body
	 *            内容
	 * @param sender
	 *            发送者
	 * @param recipients
	 *            接收者
	 * @throws MessagingException
	 * @throws AddressException
	 * */
	public synchronized void sendMailT(final String subject, final String body,
			final String sender, final String recipients) {
		new Thread(new Runnable() {

			@Override
			public void run() {

				try {
					MimeMessage message = new MimeMessage(session);
					DataHandler handler = new DataHandler(
							new ByteArrayDataSource(body.getBytes(),
									"text/plain"));

					message.setSender(new InternetAddress(sender));
					message.setSubject(subject);
					message.setDataHandler(handler);
					if (recipients.contains(",")) {
						message.setRecipients(Message.RecipientType.TO,
								InternetAddress.parse(recipients));
					} else {
						message.setRecipient(Message.RecipientType.TO,
								new InternetAddress(recipients));
					}

					Transport tran = (Transport) session.getTransport("smtp");
					tran.connect(host, 465, userName, password);
					Transport.send(message);
				} catch (Exception e) {
					Log.e("send mail error", e.getMessage());
				}
			}
		}).start();

	}
}