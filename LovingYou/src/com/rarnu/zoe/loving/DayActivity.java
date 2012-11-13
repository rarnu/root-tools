package com.rarnu.zoe.loving;

import android.app.Activity;
import android.os.Bundle;
import android.view.View;
import android.view.View.OnClickListener;
import android.view.Window;
import android.view.WindowManager;
import android.widget.Button;
import android.widget.TextView;

import com.rarnu.zoe.loving.common.DayInfo;
import com.rarnu.zoe.loving.utils.UIUtils;

public class DayActivity extends Activity implements OnClickListener {

	TextView tvDay, tvDayLine1, tvDayLine2, tvDayLine3;
	Button btnClose;

	@Override
	protected void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		requestWindowFeature(Window.FEATURE_NO_TITLE);
		setContentView(R.layout.activity_day);

		WindowManager.LayoutParams wlp = getWindow().getAttributes();
		wlp.width = (int) (UIUtils.getWidth() * 0.9);
		getWindow().setAttributes(wlp);

		tvDay = (TextView) findViewById(R.id.tvDay);
		tvDayLine1 = (TextView) findViewById(R.id.tvDayLine1);
		tvDayLine2 = (TextView) findViewById(R.id.tvDayLine2);
		tvDayLine3 = (TextView) findViewById(R.id.tvDayLine3);
		btnClose = (Button) findViewById(R.id.btnClose);

		btnClose.setOnClickListener(this);

		int day = getIntent().getIntExtra("day", 1);
		laodData(day);
	}

	private void laodData(int day) {
		DayInfo info = Global.database.queryDay(day);
		if (info.day == -1) {
			finish();
			return;
		}
		tvDay.setText(String.format(getString(R.string.day_fmt), info.day));
		tvDayLine1.setText(String.format(getString(R.string.day_line_1),
				(info.emotion == 1 ? getString(R.string.st_emotion_1)
						: getString(R.string.st_emotion_2)),
				(info.active == 1 ? getString(R.string.st_1)
						: getString(R.string.st_2))));
		tvDayLine2.setText(String.format(getString(R.string.day_line_2),
				(info.food == 1 ? getString(R.string.st_1)
						: getString(R.string.st_2)),
				(info.news == 1 ? getString(R.string.st_1)
						: getString(R.string.st_2))));
		tvDayLine3.setText(info.friend == 1 ? getString(R.string.st_friend_1)
				: getString(R.string.st_friend_2));
	}

	@Override
	public void onClick(View v) {
		switch (v.getId()) {
		case R.id.btnClose:
			finish();
			break;
		}

	}
}
