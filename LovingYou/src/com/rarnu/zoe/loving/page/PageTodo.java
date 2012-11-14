package com.rarnu.zoe.loving.page;

import android.content.Context;
import android.graphics.BitmapFactory;
import android.util.AttributeSet;
import android.widget.ImageView;
import android.widget.TextView;

import com.rarnu.zoe.loving.R;
import com.rarnu.zoe.loving.base.BasePage;

public class PageTodo extends BasePage {

	TextView tvTodoToday, tvTodo, tvDays, tvDesc;
	ImageView imgPhoto;

	public PageTodo(Context context, AttributeSet attrs, int defStyle) {
		super(context, attrs, defStyle);
	}

	public PageTodo(Context context, AttributeSet attrs) {
		super(context, attrs);
	}

	public PageTodo(Context context) {
		super(context);
	}

	@Override
	protected void requireRootLayoutId() {
		this.rootLayout = R.layout.activity_todo;

	}

	@Override
	protected void init() {
		tvTodoToday = (TextView) findViewById(R.id.tvTodoToday);
		tvTodo = (TextView) findViewById(R.id.tvTodo);
		tvDays = (TextView) findViewById(R.id.tvDays);
		tvDesc = (TextView) findViewById(R.id.tvDesc);
		imgPhoto = (ImageView) findViewById(R.id.imgPhoto);
		tvTodoToday.setText(R.string.action_day);

	}

	@Override
	public void load(String... param) {
		int day = Integer.parseInt(param[0]) + 1;
		tvDays.setText(String.format(
				getResources().getString(R.string.day_fmt), day));
		tvDesc.setText(param[0]);
	}

	@Override
	public void refresh() {

	}

	@Override
	public void delete(int index) {

	}

	public void loadImage(int res) {
		if (res == 0) {
			imgPhoto.setImageBitmap(null);
		} else {
			BitmapFactory.Options bop = new BitmapFactory.Options();
			bop.inSampleSize = 2;
			imgPhoto.setImageBitmap(BitmapFactory.decodeResource(
					getResources(), res, bop));
		}
	}
}
