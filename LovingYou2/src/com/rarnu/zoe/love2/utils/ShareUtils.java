package com.rarnu.zoe.love2.utils;

import com.rarnu.zoe.love2.R;

import android.content.Context;
import android.content.Intent;
import android.net.Uri;

public class ShareUtils {

	public static boolean shareTo(Context context, String title,
			String subject, String text, Uri uri, String[] mail) {
		Intent intent = new Intent(Intent.ACTION_SEND);
		intent.setType("image/*");
		if (subject != null) {
			intent.putExtra(Intent.EXTRA_SUBJECT, subject);
		}
		if (text != null) {
			if (text.length() > 132) {
				text = text.substring(0, 132);
			}
			text += "@"+context.getString(R.string.last_at);
			intent.putExtra(Intent.EXTRA_TEXT, text);
		}
		if (uri != null) {
			intent.putExtra(Intent.EXTRA_STREAM, uri);
		}
		if (mail != null) {
			intent.putExtra(Intent.EXTRA_EMAIL, mail);
		}
		context.startActivity(Intent.createChooser(intent, title));
		// context.startActivity(intent);
		return true;
	}

}
