package com.sbbs.me.android.utils;

import java.lang.reflect.Field;

import android.app.ActionBar;
import android.widget.FrameLayout;
import android.widget.LinearLayout;

import com.sbbs.me.android.R;

public class CustomUIUtils {

	public static void customActionBarHome(ActionBar bar) {
		try {
			Class<?> cActionBarImpl = Class.forName("com.android.internal.app.ActionBarImpl");
			Class<?> cActionBarView = Class.forName("com.android.internal.widget.ActionBarView");
			
			Field fActionView = cActionBarImpl.getDeclaredField("mActionView");
			fActionView.setAccessible(true);
			Object objActionView = fActionView.get(bar);

			Field fHomeLayout = cActionBarView.getDeclaredField("mHomeLayout");
			fHomeLayout.setAccessible(true);
			Object objHomeView = fHomeLayout.get(objActionView);
			
			Field fTitleLayout = cActionBarView.getDeclaredField("mTitleLayout");
			fTitleLayout.setAccessible(true);
			Object objTitleLayout = fTitleLayout.get(objActionView);
			
			((FrameLayout) objHomeView).setBackgroundResource(R.drawable.action_button_style);
			((LinearLayout) objTitleLayout).setBackgroundResource(R.drawable.action_button_style);
		} catch (Exception e) {

		}
	}
}
