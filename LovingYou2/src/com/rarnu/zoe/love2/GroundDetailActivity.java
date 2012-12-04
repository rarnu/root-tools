package com.rarnu.zoe.love2;

import android.os.Bundle;
import android.view.View;
import android.view.View.OnClickListener;

import com.rarnu.zoe.love2.base.BaseActivity;
import com.rarnu.zoe.love2.common.GroundInfo;
import com.rarnu.zoe.love2.comp.Title;

public class GroundDetailActivity extends BaseActivity implements
		OnClickListener {

	@Override
	protected void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		
		int id = getIntent().getIntExtra("index", -1);
		if (id == -1) {
			finish();
		}
		
		// TODO: load
		GroundInfo info = Global.database.queryGround(id);
	}
	
	@Override
	protected void setContentView() {
		setContentView(R.layout.activity_ground_detail);

	}

	@Override
	protected void initComponents() {
		super.initComponents();
		title.getBarItem(Title.BARITEM_CENTER).setText(R.string.task_detail);
		title.getBarItem(Title.BARITEM_LEFT).setIcon(R.drawable.home);
	}

	@Override
	protected void initEvents() {
		super.initEvents();
		title.getBarItem(Title.BARITEM_LEFT).setOnButtonClick(this);
	}

	@Override
	public void onClick(View v) {
		switch (v.getId()) {
		case Title.ITEM_LEFT:
			finish();
			break;
		}
	}

}
