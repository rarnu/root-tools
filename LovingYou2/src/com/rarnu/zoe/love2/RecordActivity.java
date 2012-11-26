package com.rarnu.zoe.love2;

import android.view.View;
import android.view.View.OnClickListener;

import com.rarnu.zoe.love2.base.BaseActivity;
import com.rarnu.zoe.love2.comp.Title;

public class RecordActivity extends BaseActivity implements OnClickListener {

	@Override
	protected void setContentView() {
		setContentView(R.layout.activity_record);

	}

	@Override
	protected void initComponents() {
		super.initComponents();

		title.getBarItem(Title.BARITEM_CENTER).setText(R.string.record_today);
		title.getBarItem(Title.BARITEM_RIGHT).setIcon(R.drawable.record_history);
		title.getBarItem(Title.BARITEM_LEFT).setIcon(R.drawable.home);
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
