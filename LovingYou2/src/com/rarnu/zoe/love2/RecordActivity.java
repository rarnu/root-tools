package com.rarnu.zoe.love2;

import android.os.Bundle;
import android.view.View;
import android.view.View.OnClickListener;
import android.widget.RelativeLayout;
import android.widget.TextView;

import com.rarnu.zoe.love2.base.BaseActivity;
import com.rarnu.zoe.love2.comp.Title;

public class RecordActivity extends BaseActivity implements OnClickListener {

	TextView tvRecordDesc;
	RelativeLayout layLines;
	String[] text = null;

	@Override
	protected void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		text = getResources().getStringArray(R.array.record_text);
		tvRecordDesc.setText(text[0]);
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
			break;
		}

	}
}
