package com.rarnu.tools.root.comp;

import android.content.Context;
import android.util.AttributeSet;
import android.view.View;
import android.widget.Button;
import android.widget.CheckBox;
import android.widget.RelativeLayout;

import com.rarnu.root.pp4.R;

public class DataBar extends RelativeLayout {

	// [region] field define
	Button btn1, btn2;
	CheckBox chkSelectAll;
	// [/region]
	
	// [region] constructor
	public DataBar(Context context, AttributeSet attrs, int defStyle) {
		super(context, attrs, defStyle);
		init();
	}

	public DataBar(Context context, AttributeSet attrs) {
		super(context, attrs);
		init();
	}

	public DataBar(Context context) {
		super(context);
		init();
	}
	
	// [/region]
	
	// [region] common
	public Button getButton1() {
		return btn1;
	}
	
	public Button getButton2() {
		return btn2;
	}
	
	public CheckBox getCheckBox() {
		return chkSelectAll;
	}
	
	public void setCheckBoxVisible(boolean visible) {
		chkSelectAll.setVisibility(visible ? View.VISIBLE: View.GONE);
	}
	
	public void setButton1Text(CharSequence text) {
		btn1.setText(text);
	}

	public void setButton2Text(CharSequence text) {
		btn2.setText(text);
	}
	
	public void init() {
		addView(inflate(getContext(), R.layout.dataapp_bar, null));
		btn1 = (Button) findViewById(R.id.barButton1);
		btn2 = (Button) findViewById(R.id.barButton2);
		chkSelectAll = (CheckBox) findViewById(R.id.chkSelAll);
	}
	// [/region]
}
