package com.rarnu.zoe.love2;

import android.os.Bundle;
import android.view.View;
import android.view.View.OnClickListener;
import android.widget.Button;
import android.widget.EditText;
import android.widget.Toast;

import com.rarnu.zoe.love2.api.LovingYouApi;
import com.rarnu.zoe.love2.base.BaseActivity;
import com.rarnu.zoe.love2.comp.Title;
import com.rarnu.zoe.love2.utils.MailSender;
import com.rarnu.zoe.love2.utils.MiscUtils;

public class FeedbackActivity extends BaseActivity implements OnClickListener {

	EditText etRecord;
	Button btnSubmit;

	@Override
	protected void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
	}

	@Override
	protected void setContentView() {
		setContentView(R.layout.activity_feedback);
	}

	@Override
	protected void initComponents() {
		super.initComponents();
		title.getBarItem(Title.BARITEM_CENTER).setText(R.string.feedback);
		title.getBarItem(Title.BARITEM_LEFT).setIcon(R.drawable.home);

		etRecord = (EditText) findViewById(R.id.etRecord);
		btnSubmit = (Button) findViewById(R.id.btnSubmit);
	}

	@Override
	protected void initEvents() {
		super.initEvents();
		title.getBarItem(Title.BARITEM_LEFT).setOnButtonClick(this);
		btnSubmit.setOnClickListener(this);
	}

	@Override
	public void onClick(View v) {
		switch (v.getId()) {
		case Title.ITEM_LEFT:
			LovingYouApi.saveLog(this, "FeedbackActivity", "Back");
			finish();
			break;
		case R.id.btnSubmit:
			LovingYouApi.saveLog(this, "FeedbackActivity", "SubmitFeedback");
			// feedback
			String text = etRecord.getText().toString();
			if (!text.equals("")) {
				sendMail(text);
				Toast.makeText(this, R.string.feedback_uploaded,
						Toast.LENGTH_LONG).show();
				finish();
			} else {
				Toast.makeText(this, R.string.feedback_empty, Toast.LENGTH_LONG)
						.show();
			}
			break;
		}

	}
	
	@Override
	protected void onPause() {
		MiscUtils.hideInput(this);
		super.onPause();
	}

	private void sendMail(String text) {
		String mailOwner = "lovingyouapp@gmail.com";
		String mailSender = "lovingyouuser@gmail.com";
		MailSender mail = new MailSender(mailOwner, "rarnu1985:)");
		mail.sendMailT("user_feddback", text, mailSender, mailOwner);
	}
}
