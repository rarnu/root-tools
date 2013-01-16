package com.rarnu.nfc.record;

import java.nio.charset.Charset;
import java.util.Arrays;

import android.net.Uri;
import android.nfc.NdefRecord;

import com.google.common.base.Preconditions;
import com.google.common.collect.BiMap;
import com.google.common.collect.ImmutableBiMap;
import com.google.common.primitives.Bytes;

public class UriRecord implements ParsedNdefRecord {

	public static final String RECORD_TYPE = "UriRecord";

	private static final BiMap<Byte, String> URI_PREFIX_MAP = ImmutableBiMap
			.<Byte, String> builder().put((byte) 0x00, "")
			.put((byte) 0x01, "http://www.").put((byte) 0x02, "https://www.")
			.put((byte) 0x03, "http://").put((byte) 0x04, "https://")
			.put((byte) 0x05, "tel:").put((byte) 0x06, "mailto:")
			.put((byte) 0x07, "ftp://anonymous:anonymous@")
			.put((byte) 0x08, "ftp://ftp.").put((byte) 0x09, "ftps://")
			.put((byte) 0x0A, "sftp://").put((byte) 0x0B, "smb://")
			.put((byte) 0x0C, "nfs://").put((byte) 0x0D, "ftp://")
			.put((byte) 0x0E, "dav://").put((byte) 0x0F, "news:")
			.put((byte) 0x10, "telnet://").put((byte) 0x11, "imap:")
			.put((byte) 0x12, "rtsp://").put((byte) 0x13, "urn:")
			.put((byte) 0x14, "pop:").put((byte) 0x15, "sip:")
			.put((byte) 0x16, "sips:").put((byte) 0x17, "tftp:")
			.put((byte) 0x18, "btspp://").put((byte) 0x19, "btl2cap://")
			.put((byte) 0x1A, "btgoep://").put((byte) 0x1B, "tcpobex://")
			.put((byte) 0x1C, "irdaobex://").put((byte) 0x1D, "file://")
			.put((byte) 0x1E, "urn:epc:id:").put((byte) 0x1F, "urn:epc:tag:")
			.put((byte) 0x20, "urn:epc:pat:").put((byte) 0x21, "urn:epc:raw:")
			.put((byte) 0x22, "urn:epc:").put((byte) 0x23, "urn:nfc:").build();

	private final Uri mUri;

	private UriRecord(Uri uri) {
		this.mUri = Preconditions.checkNotNull(uri);
	}

	public Uri getUri() {
		return mUri;
	}

	public static UriRecord parse(NdefRecord record) {
		short tnf = record.getTnf();
		if (tnf == NdefRecord.TNF_WELL_KNOWN) {
			return parseWellKnown(record);
		} else if (tnf == NdefRecord.TNF_ABSOLUTE_URI) {
			return parseAbsolute(record);
		}
		throw new IllegalArgumentException("Unknown TNF " + tnf);
	}

	private static UriRecord parseAbsolute(NdefRecord record) {
		byte[] payload = record.getPayload();
		Uri uri = Uri.parse(new String(payload, Charset.forName("UTF-8")));
		return new UriRecord(uri);
	}

	private static UriRecord parseWellKnown(NdefRecord record) {
		Preconditions.checkArgument(Arrays.equals(record.getType(),
				NdefRecord.RTD_URI));
		byte[] payload = record.getPayload();

		String prefix = URI_PREFIX_MAP.get(payload[0]);
		byte[] fullUri = Bytes.concat(
				prefix.getBytes(Charset.forName("UTF-8")),
				Arrays.copyOfRange(payload, 1, payload.length));
		Uri uri = Uri.parse(new String(fullUri, Charset.forName("UTF-8")));
		return new UriRecord(uri);
	}

	public static boolean isUri(NdefRecord record) {
		try {
			parse(record);
			return true;
		} catch (IllegalArgumentException e) {
			return false;
		}
	}

	@Override
	public String getNfcText() {
		return mUri.toString();
	}
}
