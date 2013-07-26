package com.sbbs.me.android.component;

import java.io.File;
import java.util.List;

import android.content.Context;
import android.graphics.Bitmap;
import android.graphics.BitmapFactory;
import android.graphics.drawable.BitmapDrawable;
import android.graphics.drawable.Drawable;
import android.os.Handler;
import android.os.Message;
import android.text.Html;
import android.text.Html.ImageGetter;
import android.util.AttributeSet;
import android.view.MotionEvent;
import android.view.View;
import android.view.View.OnTouchListener;
import android.widget.ImageView;
import android.widget.RelativeLayout;
import android.widget.TextView;

import com.rarnu.utils.DownloadUtils;
import com.rarnu.utils.UIUtils;
import com.sbbs.me.android.R;
import com.sbbs.me.android.api.SbbsMeBlock;
import com.sbbs.me.android.consts.PathDefine;
import com.sbbs.me.android.utils.HtmlUtils;
import com.sbbs.me.android.utils.MiscUtils;

public class BlockTextView extends RelativeLayout implements OnTouchListener {

	private View innerView;
	private TextView tvText;
	private ImageView ivIcon;
	private SbbsMeBlock block;
	private RainbowView rvLeft, rvRight;
	private ImageGetter ig;

	private MotionEvent touchEvent = null;
	private BitmapFactory.Options bop = null;

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

		innerView.setOnTouchListener(this);

		tvText = (TextView) innerView.findViewById(R.id.tvText);
		ivIcon = (ImageView) innerView.findViewById(R.id.ivIcon);
		rvLeft = (RainbowView) innerView.findViewById(R.id.rvLeft);
		rvRight = (RainbowView) innerView.findViewById(R.id.rvRight);

		setBackgroundResource(R.drawable.article_list_selector);
		setFocusable(true);
		setClickable(true);

		bop = new BitmapFactory.Options();
		bop.inSampleSize = 2;

		ig = new ImageGetter() {

			@Override
			public Drawable getDrawable(String source) {
				Bitmap bmp = BitmapFactory.decodeFile(PathDefine.ROOT_PATH
						+ MiscUtils.extractFileNameFromURL(source), bop);
				Drawable d = new BitmapDrawable(bmp);
				if (d != null) {
					d.setBounds(0, 0, d.getIntrinsicWidth(),
							d.getIntrinsicHeight());
				}
				return d;
			}
		};
	}

	public void setText(String html) {
		tvText.setText(Html.fromHtml(html));
		checkAndDownloadImagesT(html);
	}

	public void setCodeContent(String contents) {
		tvText.setText(contents);
	}

	public void setHeadImageUrl(String uid, String html) {
		String url = MiscUtils.extractHeadUrl(html);
		String headLocalPath = PathDefine.ROOT_PATH;
		String headLocalName = uid + ".jpg";
		DownloadUtils.downloadFileT(getContext(), ivIcon, url, headLocalPath,
				headLocalName, null);
	}

	public SbbsMeBlock getBlock() {
		return block;
	}

	public void setBlock(SbbsMeBlock block) {
		this.block = block;
	}

	@Override
	public boolean onTouch(View v, MotionEvent event) {
		this.touchEvent = event;
		return false;
	}

	/**
	 * 
	 * @return 0:left, 1:right, 2:unknown
	 */
	public int getTouchedPosition() {
		int retInt = 2;
		if (touchEvent != null) {
			float xPos = touchEvent.getX();
			if (xPos < (UIUtils.getWidth() / 5 * 2)) {
				retInt = 0;
			} else if (xPos > (UIUtils.getWidth() / 5 * 3)) {
				retInt = 1;
			}
		}
		return retInt;
	}

	public void setLeftRightCount(int left, int right) {
		rvLeft.setBlockCount(left, 0);
		rvRight.setBlockCount(right, 1);
		rvLeft.setVisibility(left == 0 ? View.GONE : View.VISIBLE);
		rvRight.setVisibility(right == 0 ? View.GONE : View.VISIBLE);
	}

	private Handler hReloadBlock = new Handler() {
		@Override
		public void handleMessage(Message msg) {
			if (msg.what == 1) {
				String html = (String) msg.obj;
				tvText.setText(Html.fromHtml(html, ig, null));
			}
			super.handleMessage(msg);
		};
	};

	private void checkAndDownloadImagesT(final String html) {

		new Thread(new Runnable() {

			@Override
			public void run() {
				List<String> imgs = HtmlUtils.getImages(html);
				for (String s : imgs) {
					String localFn = PathDefine.ROOT_PATH
							+ MiscUtils.extractFileNameFromURL(s);
					if (!new File(localFn).exists()) {
						DownloadUtils.downloadFile(s, localFn, null);
					}
				}
				Message msg = new Message();
				msg.what = 1;
				msg.obj = html;
				hReloadBlock.sendMessage(msg);
			}
		}).start();
	}

}
