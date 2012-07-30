package com.rarnu.tools.root;

import android.os.Bundle;
import android.view.View;
import android.view.View.OnClickListener;

import com.rarnu.tools.root.base.BaseActivity;

public class EggActivity extends BaseActivity implements OnClickListener {

	// [region] field define

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
		mappingTitle();
		mappingComp();
		initTitle();
		initSearchBar();
		initEvents();

	}

	@Override
	public void mappingComp() {

	}

	@Override
	public void initTitle() {

		tvName.setText(R.string.egg);
		btnLeft.setText(R.string.back);
		btnLeft.setVisibility(View.VISIBLE);
		// tbTitle.setText(getString(R.string.egg));
		// tbTitle.setLeftButtonText(getString(R.string.back));
		// tbTitle.getLeftButton().setVisibility(View.VISIBLE);

	}

	@Override
	public void initSearchBar() {

	}

	@Override
	public void initEvents() {
		btnLeft.setOnClickListener(this);

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
