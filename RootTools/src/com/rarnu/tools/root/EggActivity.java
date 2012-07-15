package com.rarnu.tools.root;

import android.app.Activity;
import android.os.Bundle;
import android.view.View;
import android.view.View.OnClickListener;

import com.rarnu.tools.root.base.ActivityIntf;
import com.rarnu.tools.root.comp.TitleBar;

public class EggActivity extends Activity implements ActivityIntf, OnClickListener {

	// [region] field define
	TitleBar tbTitle;
	// [/region]
	
	// [region] life circle
	@Override
	protected void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		setContentView(R.layout.layout_egg);
		init();
	}

	// [/region]

	// [region] init
	@Override
	public void init() {
		mappingComp();
		initTitle();
		initSearchBar();
		initEvents();

	}

	@Override
	public void mappingComp() {
		tbTitle = (TitleBar) findViewById(R.id.tbTitle);

	}

	@Override
	public void initTitle() {
		tbTitle.setText(getString(R.string.egg));
		tbTitle.setLeftButtonText(getString(R.string.back));
		tbTitle.getLeftButton().setVisibility(View.VISIBLE);

	}

	@Override
	public void initSearchBar() {
		

	}

	@Override
	public void initEvents() {
		tbTitle.getLeftButton().setOnClickListener(this);

	}
	
	// [/region]
	
	// [region] events

	@Override
	public void onClick(View v) {
		switch (v.getId()) {
		case R.id.btnLeft:
			finish();
			break;
		}
		
	}

	// [/region]
}
