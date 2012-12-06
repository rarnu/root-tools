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
import android.widget.Toast;
import android.widget.RelativeLayout.LayoutParams;
import android.widget.TextView;

import com.rarnu.zoe.love2.base.BaseActivity;
import com.rarnu.zoe.love2.common.DayInfo;
import com.rarnu.zoe.love2.comp.Checker;
import com.rarnu.zoe.love2.comp.Title;
import com.rarnu.zoe.love2.utils.UIUtils;

public class RecordActivity extends BaseActivity implements OnClickListener {

	RelativeLayout layLines, layBottomLine;
	// String[] text = null;
	ImageView imgBall;
	Checker chkE1, chkE2, chkE3, chkE4;
	TextView tvE1, tvE2, tvE3, tvE4;
	Button btnSubmit;
	EditText etRecord;

	ImageView[] imgLines = new ImageView[21];

	@Override
	protected void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		// text = getResources().getStringArray(R.array.record_text);
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

		layLines = (RelativeLayout) findViewById(R.id.layLines);
		layBottomLine = (RelativeLayout) findViewById(R.id.layBottomLine);
		imgBall = (ImageView) findViewById(R.id.imgBall);

		chkE1 = (Checker) findViewById(R.id.chkE1);
		chkE2 = (Checker) findViewById(R.id.chkE2);
		chkE3 = (Checker) findViewById(R.id.chkE3);
		chkE4 = (Checker) findViewById(R.id.chkE4);
		tvE1 = (TextView) findViewById(R.id.tvE1);
		tvE2 = (TextView) findViewById(R.id.tvE2);
		tvE3 = (TextView) findViewById(R.id.tvE3);
		tvE4 = (TextView) findViewById(R.id.tvE4);

		btnSubmit = (Button) findViewById(R.id.btnSubmit);
		etRecord = (EditText) findViewById(R.id.etRecord);

		resize();
		initEmotions();
	}

	private void resize() {
		int width = UIUtils.getWidth() - UIUtils.dipToPx(64)
				- (UIUtils.dipToPx(52) * 4);
		width = width / 3;
		RelativeLayout.LayoutParams rlp = (RelativeLayout.LayoutParams) chkE2
				.getLayoutParams();
		rlp.leftMargin = width;
		chkE2.setLayoutParams(rlp);

		rlp = (RelativeLayout.LayoutParams) chkE3.getLayoutParams();
		rlp.leftMargin = width;
		chkE3.setLayoutParams(rlp);

		rlp = (RelativeLayout.LayoutParams) chkE4.getLayoutParams();
		rlp.leftMargin = width;
		chkE4.setLayoutParams(rlp);

		rlp = (RelativeLayout.LayoutParams) tvE2.getLayoutParams();
		rlp.leftMargin = width;
		tvE2.setLayoutParams(rlp);

		rlp = (RelativeLayout.LayoutParams) tvE3.getLayoutParams();
		rlp.leftMargin = width;
		tvE3.setLayoutParams(rlp);

		rlp = (RelativeLayout.LayoutParams) tvE4.getLayoutParams();
		rlp.leftMargin = width;
		tvE4.setLayoutParams(rlp);
	}

	private void initEmotions() {
		chkE1.setYesDrawable(R.drawable.record_e1y);
		chkE1.setNoDrawable(R.drawable.record_e1n);
		chkE2.setYesDrawable(R.drawable.record_e2y);
		chkE2.setNoDrawable(R.drawable.record_e2n);
		chkE3.setYesDrawable(R.drawable.record_e3y);
		chkE3.setNoDrawable(R.drawable.record_e3n);
		chkE4.setYesDrawable(R.drawable.record_e4y);
		chkE4.setNoDrawable(R.drawable.record_e4n);

		chkE1.setStatus(Checker.STATUS_NO);
		chkE2.setStatus(Checker.STATUS_NO);
		chkE3.setStatus(Checker.STATUS_NO);
		chkE4.setStatus(Checker.STATUS_NO);
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

		btnSubmit.setOnClickListener(this);
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
			changeCheckerStatus((Checker) v);
			break;
		case R.id.btnSubmit:
			// must write something
			String txt = etRecord.getText().toString();
			if (txt.equals("")) {
				Toast.makeText(this, R.string.record_hint, Toast.LENGTH_LONG)
						.show();
				return;
			}
			long stamp = System.currentTimeMillis();
			int news = chkE1.getStatus() == Checker.STATUS_YES ? 0 : 1;
			int food = chkE2.getStatus() == Checker.STATUS_YES ? 0 : 1;
			int active = chkE3.getStatus() == Checker.STATUS_YES ? 0 : 1;
			int reading = chkE4.getStatus() == Checker.STATUS_YES ? 0 : 1;
			DayInfo info = Global.database.queryDay(Global.database.getDay());
			if ((news + food + active + reading) == 4) {
				if (info.day == -1) {
					Global.database.insertDay(stamp, 0, active, food, reading,
							news);
				}
			} else {
				Global.database
						.insertDay(stamp, 0, active, food, reading, news);
			}

			// TODO: photo?

			Global.database.insertGround(Global.database.getDay(), txt, "");
			Global.database.updateDay(Global.database.getDay(), 0);

			Intent inHis = new Intent(this, HistoryActivity.class);
			startActivity(inHis);
			finish();
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
