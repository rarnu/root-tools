package com.rarnu.zoe.love2;

import java.util.List;

import android.content.Intent;
import android.os.Bundle;
import android.view.View;
import android.view.View.OnClickListener;
import android.widget.AdapterView;
import android.widget.AdapterView.OnItemClickListener;
import android.widget.GridView;

import com.rarnu.zoe.love2.adapter.GroundAdapter;
import com.rarnu.zoe.love2.base.BaseActivity;
import com.rarnu.zoe.love2.common.GroundInfo;
import com.rarnu.zoe.love2.comp.Title;

public class GroundActivity extends BaseActivity implements OnClickListener, OnItemClickListener {

	GridView gvGround;
	List<GroundInfo> list = null;
	GroundAdapter adapter = null;
	
	@Override
	protected void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		
		list = Global.database.queryGroundHistory();
		adapter = new GroundAdapter(this, list);
		gvGround.setAdapter(adapter);
	}
	
	@Override
	protected void setContentView() {
		setContentView(R.layout.activity_ground);

	}

	@Override
	protected void initComponents() {
		super.initComponents();

		title.getBarItem(Title.BARITEM_CENTER).setText(R.string.ground);
		title.getBarItem(Title.BARITEM_LEFT).setIcon(R.drawable.home);
		
		gvGround = (GridView) findViewById(R.id.gvGround);
	}

	@Override
	protected void initEvents() {
		super.initEvents();
		title.getBarItem(Title.BARITEM_LEFT).setOnButtonClick(this);
		gvGround.setOnItemClickListener(this);
	}

	@Override
	public void onClick(View v) {
		switch (v.getId()) {
		case Title.ITEM_LEFT:
			finish();
			break;
		}

	}

	@Override
	public void onItemClick(AdapterView<?> parent, View v, int position, long id) {
		GroundInfo info = list.get(position);
		Intent inDetail = new Intent(this, GroundDetailActivity.class);
		inDetail.putExtra("index", info.id);
		startActivity(inDetail);
	}
}
