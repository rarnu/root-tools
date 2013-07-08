package com.sbbs.me.android.loader;

import android.content.Context;

import com.rarnu.devlib.base.BaseClassLoader;
import com.sbbs.me.android.api.SbbsMeAPI;
import com.sbbs.me.android.api.SbbsMeBlock;

public class SbbsBlockSender extends BaseClassLoader<String> {

	private int mode;
	private SbbsMeBlock block;
	private String text;

	public SbbsBlockSender(Context context) {
		super(context);
	}

	public void setData(SbbsMeBlock block, int mode, String text) {
		this.block = block;
		this.mode = mode;
		this.text = text;
	}

	@Override
	public String loadInBackground() {
		String ret = "";
		switch (mode) {
		case 0:
			ret = SbbsMeAPI.appendBlock(block.Id, text);
			break;
		case 1:
			ret = SbbsMeAPI.commentBlock(block.Id, text, "comment:"
					+ block.Subject);
			break;
		case 2:
			ret = SbbsMeAPI.editBlock(block.Id, text);
			break;
		}
		return ret;
	}

}
