package com.rarnu.utils.http;

public abstract class PartBase extends Part {

	private String name;
	private String contentType;
	private String charSet;
	private String transferEncoding;

	public PartBase(String name, String contentType, String charSet,
			String transferEncoding) {

		if (name == null) {
			throw new IllegalArgumentException();
		}
		this.name = name;
		this.contentType = contentType;
		this.charSet = charSet;
		this.transferEncoding = transferEncoding;
	}

	@Override
	public String getName() {
		return this.name;
	}

	@Override
	public String getContentType() {
		return this.contentType;
	}

	@Override
	public String getCharSet() {
		return this.charSet;
	}

	@Override
	public String getTransferEncoding() {
		return transferEncoding;
	}

	public void setCharSet(String charSet) {
		this.charSet = charSet;
	}

	public void setContentType(String contentType) {
		this.contentType = contentType;
	}

	public void setName(String name) {
		if (name == null) {
			throw new IllegalArgumentException();
		}
		this.name = name;
	}

	public void setTransferEncoding(String transferEncoding) {
		this.transferEncoding = transferEncoding;
	}

}