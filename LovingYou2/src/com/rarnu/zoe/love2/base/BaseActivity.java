package com.rarnu.zoe.love2.base;

import android.app.Activity;
import android.os.Bundle;

import com.rarnu.zoe.love2.R;
import com.rarnu.zoe.love2.common.Consts;
import com.rarnu.zoe.love2.comp.Title;

public abstract class BaseActivity extends Activity {

	protected Title title;
	
	@Override
	protected void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		setContentView();
		Consts.setTaskTexts(this);
		init();
	}

	protected abstract void setContentView();

	protected void init() {
		initComponents();
		initEvents();
	}

	protected void initComponents() {
		title = (Title) findViewById(R.id.title);
	}

	protected void initEvents() {

	}

}
