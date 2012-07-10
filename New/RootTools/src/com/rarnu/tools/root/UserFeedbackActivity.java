package com.rarnu.tools.root;

import android.app.Activity;
import android.os.Bundle;
import android.os.Handler;
import android.os.Message;
import android.view.View;
import android.view.View.OnClickListener;
import android.widget.EditText;
import android.widget.Toast;

import com.rarnu.tools.root.api.LogApi;
import com.rarnu.tools.root.api.MobileApi;
import com.rarnu.tools.root.base.ActivityIntf;
import com.rarnu.tools.root.comp.DataProgressBar;
import com.rarnu.tools.root.comp.TitleBar;

public class UserFeedbackActivity extends Activity implements ActivityIntf, OnClickListener {

	// [region] field define
	TitleBar tbTitle;
	EditText etFeedback;
	DataProgressBar progressFeedback;

	// [/region]

	// [region] life circle

	@Override
	protected void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		setContentView(R.layout.layout_user_feedback);
		init();
	}

	// [/region]

	// [region] events
	@Override
	public void onClick(View v) {
		switch (v.getId()) {
		case R.id.btnLeft:
			finish();
			break;
		case R.id.btnRight:
			String comment = etFeedback.getText().toString();
			if (comment.equals("")) {
				Toast.makeText(this, R.string.empty_feedback, Toast.LENGTH_LONG).show();
				return;
			}
			doSendFeedbackT(comment);
			break;
		}

	}

	// [/region]

	// [region] business logic
	private void doSendFeedbackT(final String comment) {
		progressFeedback.setAppName(getString(R.string.sending));
		progressFeedback.setVisibility(View.VISIBLE);
		tbTitle.getRightButton().setEnabled(false);
		etFeedback.setEnabled(false);

		LogApi.logUserFeedback();
		final Handler h = new Handler() {

			@Override
			public void handleMessage(Message msg) {
				if (msg.what == 1) {
					//
					if (msg.arg1 != 0) {
						// succ
						Toast.makeText(UserFeedbackActivity.this, R.string.send_feedback_succ, Toast.LENGTH_LONG)
								.show();
						finish();
					} else {
						// fail
						Toast.makeText(UserFeedbackActivity.this, R.string.send_feedback_fail, Toast.LENGTH_LONG)
								.show();
					}
					progressFeedback.setVisibility(View.GONE);
					tbTitle.getRightButton().setEnabled(true);
					etFeedback.setEnabled(true);
				}
				super.handleMessage(msg);
			}

		};

		new Thread(new Runnable() {

			@Override
			public void run() {
				boolean ret = MobileApi.userFeedback(GlobalInstance.deviceId, GlobalInstance.module,
						GlobalInstance.osVersion, GlobalInstance.mail, GlobalInstance.buildDescription, comment);
				Message msg = new Message();
				msg.what = 1;
				msg.arg1 = (ret ? 1 : 0);
				h.sendMessage(msg);

			}
		}).start();

	}

	// [/region]

	// [region] init
	@Override
	public void init() {
		mappingComp();
		initTitle();
		initSearchBar();
		initEvents();

	}

	@Override
	public void mappingComp() {
		tbTitle = (TitleBar) findViewById(R.id.tbTitle);
		etFeedback = (EditText) findViewById(R.id.etFeedback);
		progressFeedback = (DataProgressBar) findViewById(R.id.progressFeedback);
	}

	@Override
	public void initTitle() {
		tbTitle.setText(getString(R.string.feedback));
		tbTitle.setLeftButtonText(getString(R.string.back));
		tbTitle.setRightButtonText(getString(R.string.send));
		tbTitle.getLeftButton().setVisibility(View.VISIBLE);
		tbTitle.getRightButton().setVisibility(View.VISIBLE);

	}

	@Override
	public void initSearchBar() {

	}

	@Override
	public void initEvents() {
		tbTitle.getLeftButton().setOnClickListener(this);
		tbTitle.getRightButton().setOnClickListener(this);

	}

	// [/region]
}
