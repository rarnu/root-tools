package com.rarnu.tools.root.utils;

import android.content.Context;
import android.content.Intent;
import android.net.Uri;

public class ShareUtils {

	public static boolean shareTo(Context context, String title,
			String subject, String text, Uri uri, String[] mail) {
		Intent intent = new Intent(Intent.ACTION_SEND);
		intent.setType("*/*");
		if (subject != null) {
			intent.putExtra(Intent.EXTRA_SUBJECT, subject);
		}
		if (text != null) {
			intent.putExtra(Intent.EXTRA_TEXT, text);
		}
		if (uri != null) {
			intent.putExtra(Intent.EXTRA_STREAM, uri);
		}
		if (mail != null) {
			intent.putExtra(Intent.EXTRA_EMAIL, mail);
		}
		context.startActivity(Intent.createChooser(intent, title));
		return true;
	}

}
