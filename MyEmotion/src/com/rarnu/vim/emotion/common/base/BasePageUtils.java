package com.rarnu.vim.emotion.common.base;

import java.util.ArrayList;
import java.util.List;

import android.content.Context;
import android.graphics.drawable.Drawable;

import com.rarnu.vim.emotion.common.PageItem;

public abstract class BasePageUtils<T> {
	
	private Context context;
	
	public abstract List<T> getKeywordList(Context context);
	public abstract String getName(Context context, T item);
	public abstract Drawable getIcon(Context context, T item);
	public abstract Object getExtraData(Context context, T item);
	
	public BasePageUtils(Context context) {
		this.context = context;
	}

	public List<PageItem[]> buildPages() {

		List<PageItem[]> result = new ArrayList<PageItem[]>();
		List<T> keywords = getKeywordList(context);
		int pageCount = getPageCount(keywords);

		PageItem[][] items = new PageItem[pageCount][];
		for (int i = 0; i < items.length; i++) {
			items[i] = new PageItem[16];
			buildPage(i, items[i], keywords);
			result.add(items[i]);
		}

		return result;
	}

	public void buildPage(int page, PageItem[] items,
			List<T> keywords) {
		for (int i = 0; i < 16; i++) {
			items[i] = new PageItem();
		}

		for (int i = 0; i < items.length; i++) {
			if (items[i].name.equals("")) {
				if (keywords.size() != 0) {
					items[i].name = getName(context, keywords.get(0));
					items[i].image = getIcon(context, keywords.get(0));
					items[i].data = getExtraData(context, keywords.get(0));
					keywords.remove(0);
				} else {
					break;
				}
			}
		}
	}

	public int getPageCount(List<T> keyList) {

		int count = keyList.size();
		int pageCount = (count / 16);
		if (count % 16 != 0) {
			pageCount++;
		}
		return pageCount;
	}
}
