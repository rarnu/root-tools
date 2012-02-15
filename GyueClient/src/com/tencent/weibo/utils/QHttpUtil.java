package com.tencent.weibo.utils;

import java.io.File;
import java.io.UnsupportedEncodingException;
import java.util.ArrayList;
import java.util.List;

import com.tencent.weibo.beans.QParameter;

public class QHttpUtil {
	/**
	 * Return the MIME type based on the specified file name.
	 * 
	 * @param fileName
	 *            path of input file.
	 * @return MIME type.
	 */
	public static String getContentType(String fileName) {
		return null;
	}
	/**
	 * Return the MIME type based on the specified file name.
	 * 
	 * @param file File
	 * @return MIME type.
	 */
	public static String getContentType(File file) {
//		return new MimetypesFileTypeMap().getContentType(file);
//		return null;
		return "png";
	}
	
	/**
	 * Return the list of query parameters based on the specified query string.
	 * @param queryString
	 * @return the list of query parameters.
	 */
	public static List<QParameter> getQueryParameters(String queryString) {
		if (queryString.startsWith("?")) {
			queryString = queryString.substring(1);
		}

		List<QParameter> result = new ArrayList<QParameter>();

		if (queryString != null && !queryString.equals("")) {
			String[] p = queryString.split("&");
			for (String s : p) {
				if (s != null && !s.equals("")) {
					if (s.indexOf('=') > -1) {
						String[] temp = s.split("=");
						if(temp.length > 1) {
							result.add(new QParameter(temp[0], temp[1]));
						}
					}
				}
			}
		}

		return result;
	}

	/**
	 * Convert %XX
	 * 
	 * @param value
	 * @return
	 */
	public static String formParamDecode(String value) {
		int nCount = 0;
		for (int i = 0; i < value.length(); i++) {
			if (value.charAt(i) == '%') {
				i += 2;
			}
			nCount++;
		}

		byte[] sb = new byte[nCount];

		for (int i = 0, index = 0; i < value.length(); i++) {
			if (value.charAt(i) != '%') {
				sb[index++] = (byte) value.charAt(i);
			} else {
				StringBuilder sChar = new StringBuilder();
				sChar.append(value.charAt(i + 1));
				sChar.append(value.charAt(i + 2));
				sb[index++] = Integer.valueOf(sChar.toString(), 16).byteValue();
				i += 2;
			}
		}
		String decode = "";
		try {
			decode = new String(sb, "UTF-8");
		} catch (UnsupportedEncodingException e) {
			e.printStackTrace();
		}
		return decode;
	}
}
