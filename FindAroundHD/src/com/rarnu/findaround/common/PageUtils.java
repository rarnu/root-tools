package com.rarnu.findaround.common;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import com.rarnu.findaround.R;

import android.content.Context;

public class PageUtils {

	private static List<String> lstKeywordsFull = new ArrayList<String>();
	private static List<Integer> lstDefaultIcons = new ArrayList<Integer>();

	static {
		lstDefaultIcons.add(R.drawable.def0);
		lstDefaultIcons.add(R.drawable.def1);
		lstDefaultIcons.add(R.drawable.def2);
		lstDefaultIcons.add(R.drawable.def3);
		lstDefaultIcons.add(R.drawable.def4);
		lstDefaultIcons.add(R.drawable.def5);
		lstDefaultIcons.add(R.drawable.def6);
		lstDefaultIcons.add(R.drawable.def7);
		lstDefaultIcons.add(R.drawable.def8);
		lstDefaultIcons.add(R.drawable.def9);
	}

	public static int getIcon(int index) {
		try {
			return lstDefaultIcons.get(index);
		} catch (Exception e) {
			return R.drawable.def0;
		}
	}

	public static List<PageItem[]> buildPages(Context context) {

		lstKeywordsFull.clear();
		List<PageItem[]> result = new ArrayList<PageItem[]>();
		List<FixedPageItem> fixed = getFixedPageItem(context);
		List<String> keywords = Config.getKeywordsList(context);

		int pageCount = getPageCount(fixed, keywords);

		PageItem[][] items = new PageItem[pageCount][];
		for (int i = 0; i < items.length; i++) {
			items[i] = new PageItem[18];
			buildPage(i, items[i], fixed, keywords);
			result.add(items[i]);
		}

		return result;
	}

	public static void buildPage(int page, PageItem[] items,
			List<FixedPageItem> fixed, List<String> keywords) {
		for (int i = 0; i < 18; i++) {
			items[i] = new PageItem();
		}
		for (int i = fixed.size() - 1; i >= 0; i--) {
			if (fixed.get(i).page == page) {
				items[fixed.get(i).position].name = fixed.get(i).name;
				items[fixed.get(i).position].fixed = true;
				items[fixed.get(i).position].image = fixed.get(i).image;
				lstKeywordsFull.add(items[fixed.get(i).position].name);
				fixed.remove(i);
			}
		}
		for (int i = 0; i < items.length; i++) {
			if (items[i].name.equals("")) {
				if (keywords.size() != 0) {
					items[i].name = keywords.get(keywords.size() - 1);
					items[i].fixed = false;
					items[i].image = 0;
					lstKeywordsFull.add(items[i].name);
					keywords.remove(keywords.size() - 1);
				} else {
					break;
				}
			}
		}

	}

	public static List<FixedPageItem> getFixedPageItem(Context context) {
		List<FixedPageItem> result = null;
		try {
			result = new ArrayList<FixedPageItem>();
			String list = FileUtils.readAssetFile(context, "fix_item");
			String[] items = list.split("\n");
			for (String s : items) {
				result.add(FixedPageItem.build(s));
			}
		} catch (IOException e) {

		}
		return result;
	}

	public static boolean isLockedItem(Context context, String keyword) {
		boolean ret = false;
		List<FixedPageItem> list = getFixedPageItem(context);
		for (FixedPageItem item : list) {
			if (item.name.equals("")) {
				ret = true;
				break;
			}
		}
		return ret;
	}

	public static int getPageCount(List<FixedPageItem> fixList,
			List<String> keyList) {
		FixedPageItem lastItem = fixList.get(fixList.size() - 1);
		int count = fixList.size() + keyList.size();
		int pageCount = (count / 18);
		if (count % 18 != 0) {
			pageCount++;
		}
		if ((lastItem.page + 1) > pageCount) {
			pageCount = lastItem.page + 1;
		}
		return pageCount;
	}

	public static boolean isKeywordExists(String keyword) {

		return (lstKeywordsFull.indexOf(keyword) != -1);
	}
}
