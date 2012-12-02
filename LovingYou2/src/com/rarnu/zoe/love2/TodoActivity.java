package com.rarnu.zoe.love2;

import android.app.Activity;
import android.app.NotificationManager;
import android.graphics.BitmapFactory;
import android.os.Bundle;
import android.widget.ImageView;
import android.widget.TextView;

import com.rarnu.zoe.love2.common.Consts;

public class TodoActivity extends Activity {

	TextView tvTodoToday, tvTodo, tvDays, tvDesc;
	ImageView imgPhoto;

	@Override
	protected void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		setContentView(R.layout.activity_todo);

		int index = getIntent().getIntExtra("index", -1);
		int day = Global.database.getDay();
		
		if (getIntent().getAction() != null) {
			String action = getIntent().getAction();
			if (action.equals(Consts.NOTIFY_ACTION)) {
				NotificationManager manager = (NotificationManager) getSystemService(NOTIFICATION_SERVICE);
				manager.cancel(Consts.NOTIFY_ID);
				index = day - 1;
			}
		}

		tvTodoToday = (TextView) findViewById(R.id.tvTodoToday);
		tvTodo = (TextView) findViewById(R.id.tvTodo);
		tvDays = (TextView) findViewById(R.id.tvDays);
		tvDesc = (TextView) findViewById(R.id.tvDesc);
		imgPhoto = (ImageView) findViewById(R.id.imgPhoto);

		tvDays.setText(String.format(getString(R.string.day_fmt), index + 1));

		tvTodoToday.setText(index == (day - 1) ? R.string.todo_today
				: R.string.done_today);

		BitmapFactory.Options bop = new BitmapFactory.Options();
		bop.inSampleSize = 2;
		imgPhoto.setImageBitmap(BitmapFactory.decodeResource(getResources(),
				Consts.bpImgs[index], bop));
	}
}
