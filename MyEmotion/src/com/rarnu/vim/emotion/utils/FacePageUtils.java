package com.rarnu.vim.emotion.utils;

import java.util.List;

import android.content.Context;
import android.graphics.drawable.Drawable;

import com.rarnu.vim.emotion.common.EmotionFace;
import com.rarnu.vim.emotion.common.FaceLoader;
import com.rarnu.vim.emotion.common.base.BasePageUtils;

public class FacePageUtils extends BasePageUtils<EmotionFace> {


	public FacePageUtils(Context context) {
		super(context);
	}

	@Override
	public List<EmotionFace> getKeywordList(Context context) {
		return FaceLoader.faceList;
	}

	@Override
	public String getName(Context context, EmotionFace item) {
		return item.name;
	}

	@Override
	public Drawable getIcon(Context context, EmotionFace item) {
		return MiscUtils.getBitmapByAssets(context,
				"face/" + item.value);
	}

	@Override
	public Object getExtraData(Context context, EmotionFace item) {
		return item;
	}
}
