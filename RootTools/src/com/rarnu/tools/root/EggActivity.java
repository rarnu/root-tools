package com.rarnu.tools.root;

import android.app.Fragment;
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

	}

	// [/region]

	// [region] init
	



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

	@Override
	public Fragment replaceFragment() {
		// TODO Auto-generated method stub
		return null;
	}

	// [/region]
}
