package com.rarnu.zoe.loving;

import android.app.Activity;
import android.graphics.BitmapFactory;
import android.os.Bundle;
import android.widget.ImageView;
import android.widget.TextView;

public class TodoActivity extends Activity {

	TextView tvTodoToday, tvTodo, tvDays, tvDesc;
	ImageView imgPhoto;

	int[] bpImgs = new int[] { R.drawable.bp1, R.drawable.bp2, R.drawable.bp3,
			R.drawable.bp4, R.drawable.bp5, R.drawable.bp6, R.drawable.bp7,
			R.drawable.bp8, R.drawable.bp9, R.drawable.bp10, R.drawable.bp11,
			R.drawable.bp12, R.drawable.bp13, R.drawable.bp14, R.drawable.bp15,
			R.drawable.bp16, R.drawable.bp17, R.drawable.bp18, R.drawable.bp19,
			R.drawable.bp20, R.drawable.bp21 };

	@Override
	protected void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		setContentView(R.layout.activity_todo);

		int index = getIntent().getIntExtra("index", 1);
		int day = Global.database.getDay();

		tvTodoToday = (TextView) findViewById(R.id.tvTodoToday);
		tvTodo = (TextView) findViewById(R.id.tvTodo);
		tvDays = (TextView) findViewById(R.id.tvDays);
		tvDesc = (TextView) findViewById(R.id.tvDesc);
		imgPhoto = (ImageView) findViewById(R.id.imgPhoto);

		tvDays.setText(String.format(getString(R.string.day_fmt), day));
		tvTodoToday.setText(index == 1 ? R.string.todo_today
				: R.string.done_today);

		BitmapFactory.Options bop = new BitmapFactory.Options();
		bop.inSampleSize = 2;
		imgPhoto.setImageBitmap(BitmapFactory.decodeResource(getResources(),
				bpImgs[day - 1], bop));

	}
}
