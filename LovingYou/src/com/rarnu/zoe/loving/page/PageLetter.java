package com.rarnu.zoe.loving.page;

import java.io.IOException;

import android.content.Context;
import android.os.Handler;
import android.os.Message;
import android.util.AttributeSet;
import android.view.View;
import android.view.View.OnClickListener;
import android.view.inputmethod.InputMethodManager;
import android.widget.Button;
import android.widget.EditText;
import android.widget.TextView;
import android.widget.Toast;

import com.rarnu.zoe.loving.Global;
import com.rarnu.zoe.loving.R;
import com.rarnu.zoe.loving.base.BasePage;
import com.rarnu.zoe.loving.utils.WeiboUtils;
import com.weibo.sdk.android.WeiboException;
import com.weibo.sdk.android.net.RequestListener;

public class PageLetter extends BasePage implements OnClickListener,
		RequestListener {

	TextView tvPostDay;
	EditText etPostDay;
	Button btnSelectImage, btnSubmit;

	Handler hWeibo = new Handler() {
		@Override
		public void handleMessage(Message msg) {
			switch (msg.what) {
			case 1:
				etPostDay.setText("");
				hideInput();
				Toast.makeText(getContext(), R.string.send_weibo_succ,
						Toast.LENGTH_LONG).show();
				break;
			case 2:
				Toast.makeText(getContext(), R.string.send_weibo_fail,
						Toast.LENGTH_LONG).show();
				break;
			}
			super.handleMessage(msg);
		};

	};

	public PageLetter(Context context, AttributeSet attrs, int defStyle) {
		super(context, attrs, defStyle);
	}

	public PageLetter(Context context, AttributeSet attrs) {
		super(context, attrs);
	}

	public PageLetter(Context context) {
		super(context);
	}

	@Override
	protected void requireRootLayoutId() {
		this.rootLayout = R.layout.page_letter;

	}

	@Override
	protected void init() {
		tvPostDay = (TextView) findViewById(R.id.tvPostDay);
		etPostDay = (EditText) findViewById(R.id.etPostDay);
		btnSelectImage = (Button) findViewById(R.id.btnSelectImage);
		btnSubmit = (Button) findViewById(R.id.btnSubmit);

		tvPostDay.setText(String.format(
				getResources().getString(R.string.day_fmt),
				Global.database.getDay()));

		btnSelectImage.setOnClickListener(this);
		btnSubmit.setOnClickListener(this);
	}

	@Override
	public void load(String... param) {

	}

	@Override
	public void refresh() {

	}

	@Override
	public void delete(int index) {

	}

	@Override
	public void onClick(View v) {
		switch (v.getId()) {
		case R.id.btnSelectImage:
			// TODO: select image
			break;
		case R.id.btnSubmit:
			// TODO: upload
			WeiboUtils.shareArticleToSina(etPostDay.getText().toString(), "", this);
			break;
		}
	}

	@Override
	public void onComplete(String arg0) {
		hWeibo.sendEmptyMessage(1);

	}

	@Override
	public void onError(WeiboException arg0) {
		hWeibo.sendEmptyMessage(2);
	}

	@Override
	public void onIOException(IOException arg0) {
		hWeibo.sendEmptyMessage(3);
	}

	private void hideInput() {

		if (Global.activity.getCurrentFocus() != null) {
			InputMethodManager inputMethodManager = (InputMethodManager) getContext()
					.getSystemService(Context.INPUT_METHOD_SERVICE);
			inputMethodManager.hideSoftInputFromWindow(Global.activity
					.getCurrentFocus().getWindowToken(),
					InputMethodManager.HIDE_NOT_ALWAYS);

		}

	}
}
