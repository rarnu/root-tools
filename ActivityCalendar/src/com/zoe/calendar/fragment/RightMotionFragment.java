package com.zoe.calendar.fragment;

import java.util.List;

import android.os.Bundle;
import android.util.Log;
import android.view.Menu;
import android.view.MotionEvent;
import android.view.View;
import android.view.View.OnTouchListener;
import android.widget.ImageView;
import android.widget.ImageView.ScaleType;
import android.widget.LinearLayout;
import android.widget.RelativeLayout;
import android.widget.TextView;

import com.rarnu.devlib.base.BaseFragment;
import com.rarnu.utils.UIUtils;
import com.zoe.calendar.R;
import com.zoe.calendar.classes.MotionItem;
import com.zoe.calendar.database.QueryUtils;

public class RightMotionFragment extends BaseFragment implements
		OnTouchListener {

	LinearLayout layEat;
	ImageView[] ivEat;
	List<MotionItem> listMotion;
	TextView[] tvMotions;
	TextView tvFirstDay;
	RelativeLayout layMotions;

	int positionEater = 0;

	public RightMotionFragment(String tag) {
		super(tag, "");
	}

	@Override
	public int getBarTitle() {
		return R.string.app_name;
	}

	@Override
	public int getBarTitleWithPath() {
		return R.string.app_name;
	}

	@Override
	public String getCustomTitle() {
		return null;
	}

	@Override
	public void initComponents() {
		layEat = (LinearLayout) innerView.findViewById(R.id.layEat);
		tvFirstDay = (TextView) innerView.findViewById(R.id.tvFirstDay);
		layMotions = (RelativeLayout) innerView.findViewById(R.id.layMotions);

		int width = UIUtils.getWidth() - UIUtils.dipToPx(150)
				- UIUtils.dipToPx(8);
		width /= 10;
		ivEat = new ImageView[10];
		for (int i = 0; i < 10; i++) {
			ivEat[i] = new ImageView(getActivity());
			ivEat[i].setLayoutParams(new LinearLayout.LayoutParams(width, width));
			ivEat[i].setScaleType(ScaleType.CENTER_INSIDE);
			ivEat[i].setImageResource(R.drawable.bean);
			layEat.addView(ivEat[i]);
		}
		ivEat[0].setImageResource(R.drawable.motion);
	}

	@Override
	public void initEvents() {
		layEat.setOnTouchListener(this);
	}

	@Override
	public void initLogic() {

		try {
			QueryUtils.initMotion(getActivity());
		} catch (Exception e) {
		}
		try {
			listMotion = QueryUtils.queryMotion(getActivity());
			while (listMotion.size() < 10) {
				listMotion.add(new MotionItem());
			}
		} catch (Exception e) {
		}
		buildMotionBoard();
		if (listMotion.get(listMotion.size() - 1)._id == -1) {
			tvFirstDay.setText(R.string.motion_past);
		} else {
			tvFirstDay.setText(getString(R.string.motion_first_day,
					listMotion.get(listMotion.size() - 1).month + 1,
					listMotion.get(listMotion.size() - 1).day));
		}
	}

	private void buildMotionBoard() {
		// spacing 8dip

		int width = UIUtils.getWidth() - UIUtils.dipToPx(150)
				- UIUtils.dipToPx(8) - UIUtils.dipToPx(120);
		width /= 10;
		tvMotions = new TextView[10];
		// TODO: listMotion maybe null?
		for (int i = 0; i < listMotion.size(); i++) {

			tvMotions[i] = new TextView(getActivity());
			RelativeLayout.LayoutParams lp = new RelativeLayout.LayoutParams(
					width, UIUtils.dipToPx(listMotion.get(i).motion * 10));
			lp.leftMargin = ((width + UIUtils.dipToPx(12)) * (9 - i));
			lp.addRule(RelativeLayout.ALIGN_PARENT_BOTTOM, RelativeLayout.TRUE);
			tvMotions[i].setLayoutParams(lp);
			tvMotions[i].setBackgroundColor(0xFF666666);

			layMotions.addView(tvMotions[i]);
		}
	}

	@Override
	public int getFragmentLayoutResId() {
		return R.layout.fragment_right_motion;
	}

	@Override
	public String getMainActivityName() {
		return "";
	}

	@Override
	public void initMenu(Menu menu) {

	}

	@Override
	public void onGetNewArguments(Bundle bn) {

	}

	@Override
	public Bundle getFragmentState() {
		return null;
	}

	float eX = 0F;
	float newX = 0F;

	@Override
	public boolean onTouch(View v, MotionEvent event) {
		switch (event.getAction()) {
		case MotionEvent.ACTION_DOWN:
			eX = event.getX();
			break;
		case MotionEvent.ACTION_UP:
			newX = event.getX();
			if (Math.abs(newX - eX) > 80) {
				if (newX < eX) {
					// slide left
					Log.e("onTouch", "left");
					if (positionEater > 0) {
						positionEater--;
						moveEater();
					}

				} else if (newX > eX) {
					// slide right
					Log.e("onTouch", "right");
					if (positionEater < 9) {
						positionEater++;
						moveEater();
					}
				}
			}
			break;
		}
		return true;
	}

	private void moveEater() {
		for (int i = 0; i < ivEat.length; i++) {
			ivEat[i].setImageResource(i == positionEater ? R.drawable.motion
					: R.drawable.bean);
		}
		try {
			QueryUtils.updateMotion(getActivity(), positionEater + 1);
			RelativeLayout.LayoutParams rlp = (RelativeLayout.LayoutParams) tvMotions[0]
					.getLayoutParams();
			listMotion.get(0).motion = positionEater + 1;
			rlp.height = UIUtils.dipToPx(listMotion.get(0).motion * 10);
			tvMotions[0].setLayoutParams(rlp);
		} catch (Exception e) {
			
		}
	}

}
