package com.sbbs.me.android.utils;

import java.io.IOException;
import java.lang.reflect.Field;

import org.eclipse.egit.github.core.Blob;
import org.eclipse.egit.github.core.util.EncodingUtils;
import org.markdown4j.Markdown4jProcessor;

import android.app.ActionBar;
import android.app.Fragment;
import android.content.Context;
import android.graphics.drawable.ColorDrawable;
import android.graphics.drawable.Drawable;
import android.view.View;
import android.widget.FrameLayout;
import android.widget.LinearLayout;
import android.widget.RelativeLayout;

import com.rarnu.utils.UIUtils;
import com.sbbs.me.android.R;
import com.sbbs.me.android.api.SbbsMeBlock;
import com.sbbs.me.android.component.BlockTextView;

public class CustomUtils {

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

	public static void addBlock(Context context, SbbsMeBlock item,
			int leftCount, int RightCount, String headUrl,
			RelativeLayout layout, int viewId, int baseViewId,
			boolean needHead, View.OnClickListener click,
			View.OnLongClickListener longClick) {

		String userId = item.AuthorId;
		String htmlText = item.Body;
		boolean isMarkdown = item.Format.equals("Markdown");
		String text = "";
		try {
			text = isMarkdown ? (new Markdown4jProcessor().process(htmlText))
					: htmlText;
		} catch (IOException e) {
		}
		BlockTextView block = addBlock(context, text, false, userId, leftCount,
				RightCount, headUrl, layout, viewId, baseViewId, needHead,
				click, longClick);

		block.setBlock(item);

	}

	public static void addBlock(Context context, Blob item, boolean isMarkdown,
			int leftCount, int RightCount, String headUrl,
			RelativeLayout layout, int viewId, int baseViewId,
			boolean needHead, View.OnClickListener click,
			View.OnLongClickListener longClick) {
		String content = item.getContent();
		if (content == null)
			content = "";
		byte[] contents = EncodingUtils.fromBase64(content);
		String strEd = new String(contents);
		if (isMarkdown) {
			try {
				strEd = new Markdown4jProcessor().process(strEd);
				addBlock(context, strEd, false, "", leftCount, RightCount,
						headUrl, layout, viewId, baseViewId, needHead, click,
						longClick);
			} catch (IOException e) {

			}
		} else {
			addBlock(context, strEd, true, "", leftCount, RightCount, headUrl,
					layout, viewId, baseViewId, needHead, click, longClick);

		}
	}

	private static BlockTextView addBlock(Context context, String text,
			boolean isCode, String userId, int leftCount, int RightCount,
			String headUrl, RelativeLayout layout, int viewId, int baseViewId,
			boolean needHead, View.OnClickListener click,
			View.OnLongClickListener longClick) {

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
			if (isCode) {
				block.setCodeContent(text);
			} else {
				block.setText(text);
			}

			if (needHead) {
				block.setHeadImageUrl(userId, headUrl);
			}

			block.setLeftRightCount(leftCount, RightCount);
			block.setOnLongClickListener(longClick);
			block.setOnClickListener(click);

			layout.addView(block);
			layout.postInvalidate();
			return block;
		} else {
			return null;
		}
	}

	public static Drawable getEdgeFadeEffect(Context context) {
		return new ColorDrawable(context.getResources().getColor(
				R.color.google_light_green));
	}

	public static void changeFragmentTag(Fragment f, String newTag) {
		try {
			Field fTag = Fragment.class.getDeclaredField("mTag");
			fTag.setAccessible(true);
			fTag.set(f, newTag);
		} catch (Exception e) {

		}
	}
}
