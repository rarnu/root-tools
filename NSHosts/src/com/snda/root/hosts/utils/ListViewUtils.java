package com.snda.root.hosts.utils;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import android.widget.CheckBox;
import android.widget.ListView;
import android.widget.RelativeLayout;

public class ListViewUtils {

	public static void setListSelected(ListView lv, List<Map<String, String>> list, boolean sel) {
		
		if (list == null || list.size() == 0) {
			return;
		}
		for (Map<String, String> m : list) {
			m.remove("CHECKED");
			m.put("CHECKED", sel ? "true":"false");
		}
		
		for (int i = 0; i < lv.getCount(); i++) {

			RelativeLayout viewItem = (RelativeLayout) lv.getChildAt(i);
			if (viewItem != null) {
				for (int j = 0; j < viewItem.getChildCount(); j++) {
					if (viewItem.getChildAt(j) instanceof CheckBox) {
						((CheckBox) viewItem.getChildAt(j)).setChecked(sel);
					}
				}
			}
		}
	}
	
	public static List<Map<String, String>> getListSelectedItems(List<Map<String, String>> list) {
		List<Map<String, String>> result = new ArrayList<Map<String, String>>();
		for (Map<String, String> m: list) {
			if (m.get("CHECKED").equals("true")) {
				result.add(m);
			}
		}
		return result;
	}
	
//	@SuppressWarnings("unchecked")
//	public static List<Map<String, String>> getListSelectedItems(ListView lv) {
//		List<Map<String, String>> result = new ArrayList<Map<String, String>>();
//
//		for (int i = 0; i < lv.getCount(); i++) {
//
//			RelativeLayout viewItem = (RelativeLayout) lv
//					.getChildAt(i);
//			if (viewItem != null) {
//				for (int j = 0; j < viewItem.getChildCount(); j++) {
//					if (viewItem.getChildAt(j) instanceof CheckBox) {
//						if (((CheckBox) viewItem.getChildAt(j)).isChecked()) {
//							result.add((Map<String, String>) lv
//									.getItemAtPosition(i));
//						}
//
//					}
//				}
//			}
//		}
//
//		return result;
//	}
	
	public static int getListSelectedCount(List<Map<String, String>> list) {
		int result = 0;
		if (list == null || list.size() == 0) {
			return 0;
		}
		for (Map<String, String> m: list) {
			if (m.get("CHECKED").equals("true")) {
				result++;
			}
		}
		return result;
	}
}
