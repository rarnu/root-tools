package com.sbbs.me.android.component;

import android.content.Context;
import android.os.Environment;
import android.text.Html;
import android.util.AttributeSet;
import android.view.View;
import android.widget.ImageView;
import android.widget.RelativeLayout;
import android.widget.TextView;

import com.rarnu.utils.DownloadUtils;
import com.sbbs.me.android.R;
import com.sbbs.me.android.utils.MiscUtils;

public class BlockTextView extends RelativeLayout {

	private View innerView;
	private TextView tvText;
	private ImageView ivIcon;

	public BlockTextView(Context context, AttributeSet attrs, int defStyle) {
		super(context, attrs, defStyle);
		init();
	}

	public BlockTextView(Context context, AttributeSet attrs) {
		super(context, attrs);
		init();
	}

	public BlockTextView(Context context) {
		super(context);
		init();
	}

	private void init() {
		innerView = inflate(getContext(), R.layout.comp_blocktextview, null);
		innerView.setLayoutParams(new RelativeLayout.LayoutParams(
				RelativeLayout.LayoutParams.MATCH_PARENT,
				RelativeLayout.LayoutParams.WRAP_CONTENT));
		addView(innerView);

		tvText = (TextView) innerView.findViewById(R.id.tvText);
		ivIcon = (ImageView) innerView.findViewById(R.id.ivIcon);

		setBackgroundResource(R.drawable.article_list_selector);
		setFocusable(true);
		setClickable(true);
	}

	public void setText(String html) {
		tvText.setText(Html.fromHtml(html));
	}

	public void setHeadImageUrl(String uid, String html) {
		String url = MiscUtils.extractHeadUrl(html);
		String headLocalPath = Environment.getExternalStorageDirectory()
				.getPath() + "/.sbbs/";
		String headLocalName = uid + ".jpg";
		DownloadUtils.downloadFileT(getContext(), ivIcon, url, headLocalPath,
				headLocalName, null);
	}

}
