package com.yugioh.android.loader;

import java.util.List;

import android.content.Context;

import com.rarnu.devlib.base.BaseLoader;
import com.yugioh.android.classes.RecommandInfo;
import com.yugioh.android.utils.YGOAPI;

public class RecommandLoader extends BaseLoader<RecommandInfo> {

	public RecommandLoader(Context context) {
		super(context);
	}

	@Override
	public List<RecommandInfo> loadInBackground() {
		return YGOAPI.getRecommands();
	}

}
