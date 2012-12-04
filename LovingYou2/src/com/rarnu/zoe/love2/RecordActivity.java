package com.rarnu.zoe.love2;

import android.content.Intent;
import android.graphics.Color;
import android.os.Bundle;
import android.view.View;
import android.view.View.OnClickListener;
import android.widget.Button;
import android.widget.EditText;
import android.widget.ImageView;
import android.widget.RelativeLayout;
import android.widget.RelativeLayout.LayoutParams;
import android.widget.TextView;

import com.rarnu.zoe.love2.base.BaseActivity;
import com.rarnu.zoe.love2.comp.Checker;
import com.rarnu.zoe.love2.comp.Title;
import com.rarnu.zoe.love2.utils.UIUtils;

public class RecordActivity extends BaseActivity implements OnClickListener {

	TextView tvRecordDesc;
	RelativeLayout layLines, layBottomLine;
	String[] text = null;
	ImageView imgBall;
	Checker chkE1, chkE2, chkE3, chkE4, chkE5;
	Button btnSubmit;
	EditText etRecord;

	ImageView[] imgLines = new ImageView[21];

	@Override
	protected void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		text = getResources().getStringArray(R.array.record_text);
		tvRecordDesc.setText(text[0]);
		build21Lines(Global.database.getDay());
	}

	@Override
	protected void setContentView() {
		setContentView(R.layout.activity_record);

	}

	@Override
	protected void initComponents() {
		super.initComponents();

		title.getBarItem(Title.BARITEM_CENTER).setText(R.string.record_today);
		title.getBarItem(Title.BARITEM_RIGHT)
				.setIcon(R.drawable.record_history);
		title.getBarItem(Title.BARITEM_LEFT).setIcon(R.drawable.home);

		tvRecordDesc = (TextView) findViewById(R.id.tvRecordDesc);
		layLines = (RelativeLayout) findViewById(R.id.layLines);
		layBottomLine = (RelativeLayout) findViewById(R.id.layBottomLine);
		imgBall = (ImageView) findViewById(R.id.imgBall);

		chkE1 = (Checker) findViewById(R.id.chkE1);
		chkE2 = (Checker) findViewById(R.id.chkE2);
		chkE3 = (Checker) findViewById(R.id.chkE3);
		chkE4 = (Checker) findViewById(R.id.chkE4);
		chkE5 = (Checker) findViewById(R.id.chkE5);

		btnSubmit = (Button) findViewById(R.id.btnSubmit);
		etRecord = (EditText) findViewById(R.id.etRecord);
	}

	@Override
	protected void initEvents() {
		super.initEvents();
		title.getBarItem(Title.BARITEM_LEFT).setOnButtonClick(this);
		title.getBarItem(Title.BARITEM_RIGHT).setOnButtonClick(this);

		chkE1.setOnButtonClick(this);
		chkE2.setOnButtonClick(this);
		chkE3.setOnButtonClick(this);
		chkE4.setOnButtonClick(this);
		chkE5.setOnButtonClick(this);

		btnSubmit.setOnClickListener(this);
		
		etRecord.setOnClickListener(this);
	}

	@Override
	public void onClick(View v) {
		switch (v.getId()) {
		case Title.ITEM_LEFT:
			finish();
			break;
		case Title.ITEM_RIGHT:
			Intent inHistory = new Intent(this, HistoryActivity.class);
			startActivity(inHistory);
			break;
		case R.id.chkE1:
		case R.id.chkE2:
		case R.id.chkE3:
		case R.id.chkE4:
		case R.id.chkE5:
			changeCheckerStatus((Checker) v);
			break;
		case R.id.btnSubmit:
			long stamp = System.currentTimeMillis();
			int emotion = chkE1.getStatus() == Checker.STATUS_YES ? 0 : 1;
			int active = chkE2.getStatus() == Checker.STATUS_YES ? 0 : 1;
			int food = chkE3.getStatus() == Checker.STATUS_YES ? 0 : 1;
			int friend = chkE4.getStatus() == Checker.STATUS_YES ? 0 : 1;
			int news = chkE5.getStatus() == Checker.STATUS_YES ? 0 : 1;
			Global.database.insertDay(stamp, emotion, active, food, friend, news);
			Intent inHis = new Intent(this, HistoryActivity.class);
			startActivity(inHis);
			finish();
			break;
		case R.id.etRecord:
			Intent inInput = new Intent(this, InputActivity.class);
			startActivityForResult(inInput, 0);
			break;
		}

	}
	
	@Override
	protected void onActivityResult(int requestCode, int resultCode, Intent data) {
		if (resultCode != RESULT_OK) {
			return;
		}
		
		switch (requestCode) {
		case 0:
			break;
		}
		
	}

	private void changeCheckerStatus(Checker chk) {
		chk.setStatus(chk.getStatus() == Checker.STATUS_YES ? Checker.STATUS_NO
				: Checker.STATUS_YES);
	}

	private void build21Lines(int day) {

		int width = UIUtils.getWidth() / 21;
		int ballLeft = 0;
		for (int i = 0; i < 21; i++) {
			imgLines[i] = new ImageView(this);
			RelativeLayout.LayoutParams rlp = new RelativeLayout.LayoutParams(
					LayoutParams.WRAP_CONTENT, LayoutParams.WRAP_CONTENT);
			rlp.addRule(RelativeLayout.ALIGN_PARENT_BOTTOM, RelativeLayout.TRUE);
			if ((i + 1) % 5 == 0) {
				imgLines[i].setBackgroundResource(R.drawable.record_bl);
				rlp.height = UIUtils.dipToPx(12);
				rlp.leftMargin = (width * (i + 1)) - UIUtils.dipToPx(2);
				ballLeft = rlp.leftMargin - UIUtils.dipToPx(4);
				TextView tvNumber = new TextView(this);
				RelativeLayout.LayoutParams trlp = new RelativeLayout.LayoutParams(
						LayoutParams.WRAP_CONTENT, LayoutParams.WRAP_CONTENT);
				trlp.leftMargin = rlp.leftMargin
						- UIUtils.dipToPx(i < 5 ? 1 : 3);
				trlp.addRule(RelativeLayout.CENTER_VERTICAL,
						RelativeLayout.TRUE);
				tvNumber.setLayoutParams(trlp);
				tvNumber.setTextColor(Color.BLACK);
				tvNumber.setTextSize(8);
				tvNumber.setText(String.valueOf(i + 1));
				layBottomLine.addView(tvNumber);
			} else {
				imgLines[i].setBackgroundResource(R.drawable.record_sl);
				rlp.height = UIUtils.dipToPx(8);
				rlp.leftMargin = (width * (i + 1)) - UIUtils.dipToPx(1);
				ballLeft = rlp.leftMargin - UIUtils.dipToPx(5);
			}
			imgLines[i].setLayoutParams(rlp);
			layLines.addView(imgLines[i]);
			if ((i + 1) == day) {
				moveBall(ballLeft);
			}
		}
	}

	private void moveBall(int marginLeft) {
		RelativeLayout.LayoutParams rlp = (RelativeLayout.LayoutParams) imgBall
				.getLayoutParams();
		rlp.leftMargin = marginLeft;
		imgBall.setLayoutParams(rlp);
	}
}
