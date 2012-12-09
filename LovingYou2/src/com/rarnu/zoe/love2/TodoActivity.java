package com.rarnu.zoe.love2;

import android.app.Activity;
import android.app.NotificationManager;
import android.content.Intent;
import android.graphics.BitmapFactory;
import android.os.Bundle;
import android.view.View;
import android.view.View.OnClickListener;
import android.widget.ImageView;
import android.widget.RelativeLayout;
import android.widget.TextView;

import com.rarnu.zoe.love2.common.Consts;

public class TodoActivity extends Activity implements OnClickListener {

	TextView tvTodoToday, tvTodo, tvDays, tvDesc;
	ImageView imgPhoto;
	RelativeLayout layHotArea, layBackArea;

	@Override
	protected void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		setContentView(R.layout.activity_todo);

		Consts.setTaskTexts(this);

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

		layHotArea = (RelativeLayout) findViewById(R.id.layHotArea);
		layBackArea = (RelativeLayout) findViewById(R.id.layBackArea);

		tvDays.setText(String.format(getString(R.string.day_fmt), index + 1));

		tvTodoToday.setText(index == (day - 1) ? R.string.todo_today
				: R.string.done_today);

		tvTodo.setText(Consts.taskTitle[index]);
		tvDesc.setText(Consts.taskText[index]);

		BitmapFactory.Options bop = new BitmapFactory.Options();
		bop.inSampleSize = 2;
		imgPhoto.setImageBitmap(BitmapFactory.decodeResource(getResources(),
				Consts.bpImgs[index], bop));

		layHotArea.setOnClickListener(this);
		layBackArea.setOnClickListener(this);
	}

	@Override
	public void onClick(View v) {
		switch (v.getId()) {
		case R.id.layHotArea:
			Intent inRecord = new Intent(this, RecordActivity.class);
			startActivity(inRecord);
			break;
		case R.id.layBackArea:
			finish();
			break;
		}

	}
}
