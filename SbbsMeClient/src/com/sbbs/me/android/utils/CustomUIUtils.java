package com.sbbs.me.android.utils;

import java.lang.reflect.Field;

import org.markdown4j.Markdown4jProcessor;

import android.app.ActionBar;
import android.content.Context;
import android.view.View;
import android.widget.FrameLayout;
import android.widget.LinearLayout;
import android.widget.RelativeLayout;

import com.rarnu.utils.UIUtils;
import com.sbbs.me.android.R;
import com.sbbs.me.android.api.SbbsMeBlock;
import com.sbbs.me.android.component.BlockTextView;

public class CustomUIUtils {

	public static void customActionBarHome(ActionBar bar) {
		try {
			Class<?> cActionBarImpl = Class
					.forName("com.android.internal.app.ActionBarImpl");
			Class<?> cActionBarView = Class
					.forName("com.android.internal.widget.ActionBarView");

			Field fActionView = cActionBarImpl.getDeclaredField("mActionView");
			fActionView.setAccessible(true);
			Object objActionView = fActionView.get(bar);

			Field fHomeLayout = cActionBarView.getDeclaredField("mHomeLayout");
			fHomeLayout.setAccessible(true);
			Object objHomeView = fHomeLayout.get(objActionView);

			Field fTitleLayout = cActionBarView
					.getDeclaredField("mTitleLayout");
			fTitleLayout.setAccessible(true);
			Object objTitleLayout = fTitleLayout.get(objActionView);

			((FrameLayout) objHomeView)
					.setBackgroundResource(R.drawable.action_button_style);
			((LinearLayout) objTitleLayout)
					.setBackgroundResource(R.drawable.action_button_style);
		} catch (Exception e) {

		}
	}

	public static void addBlock(Context context, SbbsMeBlock item, int leftCount, int RightCount, String headUrl,
			RelativeLayout layout, int viewId, int baseViewId,
			boolean needHead, View.OnClickListener click,
			View.OnLongClickListener longClick) {

		String userId = item.AuthorId;
		String htmlText = item.Body;
		boolean isMarkdown = item.Format.equals("Markdown");

		if (context != null) {
			BlockTextView block = new BlockTextView(context);
			block.setId(viewId);
			RelativeLayout.LayoutParams rllp = new RelativeLayout.LayoutParams(
					LinearLayout.LayoutParams.MATCH_PARENT,
					LinearLayout.LayoutParams.WRAP_CONTENT);
			if (viewId > baseViewId) {
				rllp.addRule(RelativeLayout.BELOW, viewId - 1);
			}
			rllp.bottomMargin = UIUtils.dipToPx(4);
			block.setLayoutParams(rllp);
			try {
				block.setText(isMarkdown ? (new Markdown4jProcessor()
						.process(htmlText)) : htmlText);
				if (needHead) {
					block.setHeadImageUrl(userId, headUrl);
				}
			} catch (Exception e) {

			}
			block.setLeftRightCount(leftCount, RightCount);
			block.setBlock(item);
			block.setOnLongClickListener(longClick);
			block.setOnClickListener(click);
			
			layout.addView(block);
			layout.postInvalidate();
		}
	}
}
