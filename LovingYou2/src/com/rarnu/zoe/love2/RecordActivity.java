package com.rarnu.zoe.love2;

import com.rarnu.zoe.love2.base.BaseActivity;
import com.rarnu.zoe.love2.comp.Title;

public class RecordActivity extends BaseActivity {

	@Override
	protected void setContentView() {
		setContentView(R.layout.activity_record);

	}

	@Override
	protected void initComponents() {
		super.initComponents();

		title.getBarItem(Title.BARITEM_CENTER).setText(R.string.record_today);
	}
}
