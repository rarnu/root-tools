package com.rarnu.vim.emotion.utils;

import java.util.List;

import com.rarnu.vim.emotion.common.base.BasePageUtils;

import android.content.Context;
import android.content.Intent;
import android.content.pm.PackageManager;
import android.content.pm.ResolveInfo;
import android.graphics.drawable.Drawable;

public class SharePageUtils extends BasePageUtils<ResolveInfo> {

	public SharePageUtils(Context context) {
		super(context);
	}

	@Override
	public List<ResolveInfo> getKeywordList(Context context) {
		Intent shareIntent = new Intent(Intent.ACTION_SEND);
		shareIntent.setType("image/*");
		PackageManager pm = context.getPackageManager();
		return pm.queryIntentActivities(shareIntent, 0);
	}

	@Override
	public String getName(Context context, ResolveInfo item) {
		return item.loadLabel(context.getPackageManager()).toString();
	}

	@Override
	public Drawable getIcon(Context context, ResolveInfo item) {
		return item.loadIcon(context.getPackageManager());
	}

	@Override
	public Object getExtraData(Context context, ResolveInfo item) {
		return item;
	}
}
