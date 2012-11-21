package com.rarnu.zoe.love2;

import com.rarnu.zoe.love2.base.BaseActivity;
import com.rarnu.zoe.love2.comp.Title;

public class MainActivity extends BaseActivity {

	@Override
	protected void setContentView() {
		setContentView(R.layout.activity_main);
	}

	@Override
	protected void initComponents() {
		super.initComponents();

		title.getBarItem(Title.BARITEM_CENTER).setIcon(R.drawable.ic_launcher);
		title.getBarItem(Title.BARITEM_CENTER).setText(R.string.all_task);
	}
}
