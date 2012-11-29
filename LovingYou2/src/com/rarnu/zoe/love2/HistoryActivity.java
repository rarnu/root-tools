package com.rarnu.zoe.love2;

import android.os.Bundle;
import android.view.View;
import android.view.View.OnClickListener;
import android.widget.GridView;
import android.widget.RelativeLayout;
import android.widget.TextView;

import com.rarnu.zoe.love2.base.BaseActivity;
import com.rarnu.zoe.love2.comp.Title;

public class HistoryActivity extends BaseActivity implements OnClickListener {

	RelativeLayout layTree;
	TextView tvTree;
	TextView tvValue5, tvValue4, tvValue3, tvValue2, tvValue1;
	GridView gvHistory;

	@Override
	protected void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		buildTree(Global.database.getDay());
	}

	@Override
	protected void setContentView() {
		setContentView(R.layout.activity_history);
	}

	@Override
	protected void initComponents() {
		super.initComponents();
		title.getBarItem(Title.BARITEM_CENTER).setText(R.string.history);
		title.getBarItem(Title.BARITEM_LEFT).setIcon(R.drawable.home);

		layTree = (RelativeLayout) findViewById(R.id.layTree);
		tvTree = (TextView) findViewById(R.id.tvTree);
		tvValue5 = (TextView) findViewById(R.id.tvValue5);
		tvValue4 = (TextView) findViewById(R.id.tvValue4);
		tvValue3 = (TextView) findViewById(R.id.tvValue3);
		tvValue2 = (TextView) findViewById(R.id.tvValue2);
		tvValue1 = (TextView) findViewById(R.id.tvValue1);
		gvHistory = (GridView) findViewById(R.id.gvHistory);

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
	
	private void buildTree(int day) {
		// TODO: build tree
	}

}
