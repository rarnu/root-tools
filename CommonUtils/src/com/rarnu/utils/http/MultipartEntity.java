package com.rarnu.utils.http;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.Random;

import org.apache.http.Header;
import org.apache.http.entity.AbstractHttpEntity;
import org.apache.http.message.BasicHeader;
import org.apache.http.params.HttpParams;
import org.apache.http.protocol.HTTP;
import org.apache.http.util.EncodingUtils;

public class MultipartEntity extends AbstractHttpEntity {

	private static final String MULTIPART_FORM_CONTENT_TYPE = "multipart/form-data";
	public static final String MULTIPART_BOUNDARY = "http.method.multipart.boundary";

	private static byte[] MULTIPART_CHARS = EncodingUtils
			.getAsciiBytes("-_1234567890abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ");

	private static byte[] generateMultipartBoundary() {
		Random rand = new Random();
		byte[] bytes = new byte[rand.nextInt(11) + 30];
		for (int i = 0; i < bytes.length; i++) {
			bytes[i] = MULTIPART_CHARS[rand.nextInt(MULTIPART_CHARS.length)];
		}
		return bytes;
	}

	protected Part[] parts;

	private byte[] multipartBoundary;

	private HttpParams params;

	private boolean contentConsumed = false;

	public MultipartEntity(Part[] parts, HttpParams params) {
		if (parts == null) {
			throw new IllegalArgumentException("parts cannot be null");
		}
		if (params == null) {
			throw new IllegalArgumentException("params cannot be null");
		}
		this.parts = parts;
		this.params = params;
	}

	public MultipartEntity(Part[] parts) {
		setContentType(MULTIPART_FORM_CONTENT_TYPE);
		if (parts == null) {
			throw new IllegalArgumentException("parts cannot be null");
		}
		this.parts = parts;
		this.params = null;
	}

	protected byte[] getMultipartBoundary() {
		if (multipartBoundary == null) {
			String temp = null;
			if (params != null) {
				temp = (String) params.getParameter(MULTIPART_BOUNDARY);
			}
			if (temp != null) {
				multipartBoundary = EncodingUtils.getAsciiBytes(temp);
			} else {
				multipartBoundary = generateMultipartBoundary();
			}
		}
		return multipartBoundary;
	}

	public boolean isRepeatable() {
		for (int i = 0; i < parts.length; i++) {
			if (!parts[i].isRepeatable()) {
				return false;
			}
		}
		return true;
	}

	public void writeTo(OutputStream out) throws IOException {
		Part.sendParts(out, parts, getMultipartBoundary());
	}

	@Override
	public Header getContentType() {
		StringBuffer buffer = new StringBuffer(MULTIPART_FORM_CONTENT_TYPE);
		buffer.append("; boundary=");
		buffer.append(EncodingUtils.getAsciiString(getMultipartBoundary()));
		return new BasicHeader(HTTP.CONTENT_TYPE, buffer.toString());

	}

	public long getContentLength() {
		try {
			return Part.getLengthOfParts(parts, getMultipartBoundary());
		} catch (Exception e) {
			return 0;
		}
	}

	public InputStream getContent() throws IOException, IllegalStateException {
		if (!isRepeatable() && this.contentConsumed) {
			throw new IllegalStateException("Content has been consumed");
		}
		this.contentConsumed = true;

		ByteArrayOutputStream baos = new ByteArrayOutputStream();
		Part.sendParts(baos, this.parts, this.multipartBoundary);
		ByteArrayInputStream bais = new ByteArrayInputStream(baos.toByteArray());
		return bais;
	}

	public boolean isStreaming() {
		return false;
	}
}