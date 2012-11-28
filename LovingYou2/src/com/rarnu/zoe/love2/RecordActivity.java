package com.rarnu.zoe.love2;

import android.content.Intent;
import android.graphics.Color;
import android.os.Bundle;
import android.view.View;
import android.view.View.OnClickListener;
import android.widget.ImageView;
import android.widget.RelativeLayout;
import android.widget.RelativeLayout.LayoutParams;
import android.widget.TextView;

import com.rarnu.zoe.love2.base.BaseActivity;
import com.rarnu.zoe.love2.comp.Title;
import com.rarnu.zoe.loving.utils.UIUtils;

public class RecordActivity extends BaseActivity implements OnClickListener {

	TextView tvRecordDesc;
	RelativeLayout layLines, layBottomLine;
	String[] text = null;
	ImageView imgBall;

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
	}

	@Override
	protected void initEvents() {
		super.initEvents();
		title.getBarItem(Title.BARITEM_LEFT).setOnButtonClick(this);
		title.getBarItem(Title.BARITEM_RIGHT).setOnButtonClick(this);
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
		}

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
		RelativeLayout.LayoutParams rlp = (RelativeLayout.LayoutParams) imgBall.getLayoutParams();
		rlp.leftMargin = marginLeft;
		imgBall.setLayoutParams(rlp);
	}
}
