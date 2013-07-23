package com.sbbs.me.android.loader;

import android.content.Context;

import com.rarnu.devlib.base.BaseClassLoader;
import com.sbbs.me.android.api.SbbsMeAPI;

public class SbbsFeedbackSender extends BaseClassLoader<String> {

	private String text;
	private String userId;
	private String email;
	
	public SbbsFeedbackSender(Context context) {
		super(context);
	}
	
	public void setFeedback(String userId, String email, String text) {
		this.text = text;
		this.userId = userId;
		this.email = email;
	}

	@Override
	public String loadInBackground() {
		String ret = SbbsMeAPI.feedback(userId, email, text);
		return ret;
	}

}
