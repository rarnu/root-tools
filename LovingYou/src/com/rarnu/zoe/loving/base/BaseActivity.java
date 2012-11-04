package com.rarnu.zoe.loving.base;

import android.app.Activity;
import android.os.Bundle;

public abstract class BaseActivity extends Activity {

	@Override
	protected void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		setContentView();
		init();
	}

	protected abstract void setContentView();

	protected void init() {
		initComponents();
		initEvents();
	}

	protected void initComponents() {
		
	}

	protected void initEvents() {

	}

}
