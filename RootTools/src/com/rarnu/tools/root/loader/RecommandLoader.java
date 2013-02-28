package com.rarnu.tools.root.loader;

import java.util.List;

import android.content.Context;

import com.rarnu.devlib.base.BaseLoader;
import com.rarnu.tools.root.api.MobileApi;
import com.rarnu.tools.root.common.RecommandInfo;

public class RecommandLoader extends BaseLoader<RecommandInfo> {

	public RecommandLoader(Context context) {
		super(context);
	}

	@Override
	public List<RecommandInfo> loadInBackground() {
		return MobileApi.getRecommand();
	}

}
