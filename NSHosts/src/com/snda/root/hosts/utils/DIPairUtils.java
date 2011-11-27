package com.snda.root.hosts.utils;

import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.snda.root.hosts.GlobalInstance;
import com.snda.root.hosts.dns.record.Address;

public class DIPairUtils {

	public static List<Map<String, String>> toPairList(String domain,
			List<Address> rrList) {
		List<Map<String, String>> result = null;
		if (rrList != null) {
			if (rrList.size() > 0) {
				result = new ArrayList<Map<String, String>>();
				for (Address dr : rrList) {

					if (!dr.getRRName().equals(domain)) {
						Map<String, String> di = new HashMap<String, String>();
						di.put("IP", dr.toByteString());
						di.put("DOMAIN", domain);
						di.put("CHECKED", GlobalInstance.autoSelect ? "true"
								: "false");

						if (result.indexOf(di) == -1) {
							result.add(di);
						}
					}
					Map<String, String> di = new HashMap<String, String>();
					di.put("IP", dr.toByteString());
					di.put("DOMAIN", dr.getRRName());
					di.put("CHECKED", GlobalInstance.autoSelect ? "true"
							: "false");

					if (result.indexOf(di) == -1) {
						result.add(di);
					}

				}
			}
		}
		return result;
	}

	public static List<Map<String, String>> toPairList(String fileName) {
		List<Map<String, String>> result = null;
		try {
			List<String> lst = FileUtils.readFile(fileName);
			if (lst != null) {
				if (lst.size() > 0) {
					result = new ArrayList<Map<String, String>>();
					for (String s : lst) {
						if (!s.trim().equals("")) {
							if (!s.startsWith("#")) {
								s = s.replace("\t", " ")
										.replaceAll("\\s+", " ");
								String[] ss = s.split(" ");
								if (ss.length == 2) {
									Map<String, String> di = new HashMap<String, String>();
									di.put("IP", ss[0]);
									di.put("DOMAIN", ss[1]);
									di.put("CHECKED", "false");

									if (result.indexOf(di) == -1) {
										result.add(di);
									}
								}
							}
						}
					}
				}
			}
		} catch (IOException e) {

		}

		return result;
	}

	public static void mergePairLists(List<Map<String, String>> dest,
			List<Map<String, String>> source) {
		for (Map<String, String> obj : source) {
			if (dest.indexOf(obj) == -1) {
				dest.add(obj);
			}
		}
	}
}
