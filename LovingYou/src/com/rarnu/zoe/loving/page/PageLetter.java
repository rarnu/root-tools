package com.rarnu.zoe.loving.page;

import android.content.Context;
import android.util.AttributeSet;
import android.widget.TextView;

import com.rarnu.zoe.loving.Global;
import com.rarnu.zoe.loving.R;
import com.rarnu.zoe.loving.base.BasePage;

public class PageLetter extends BasePage {

	TextView tvPostDay;
	
	public PageLetter(Context context, AttributeSet attrs, int defStyle) {
		super(context, attrs, defStyle);
	}
	
	public PageLetter(Context context, AttributeSet attrs) {
		super(context, attrs);
	}
	
	public PageLetter(Context context) {
		super(context);
	}

	@Override
	protected void requireRootLayoutId() {
		this.rootLayout = R.layout.page_letter;

	}

	@Override
	protected void init() {
		tvPostDay = (TextView) findViewById(R.id.tvPostDay);
		tvPostDay.setText(String.format(getResources().getString(R.string.day_fmt), Global.database.getDay()));
	}

	@Override
	public void load(String... param) {

	}

	@Override
	public void refresh() {

	}

	@Override
	public void delete(int index) {

	}

}
