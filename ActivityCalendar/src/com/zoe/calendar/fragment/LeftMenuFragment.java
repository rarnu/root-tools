package com.zoe.calendar.fragment;

import java.util.ArrayList;
import java.util.List;

import android.content.Intent;
import android.os.Bundle;
import android.os.Handler;
import android.os.Message;
import android.util.Log;
import android.view.Menu;
import android.view.MotionEvent;
import android.view.View;
import android.view.View.OnTouchListener;
import android.widget.AdapterView;
import android.widget.ImageView;
import android.widget.LinearLayout;
import android.widget.RelativeLayout;
import android.widget.TextView;
import android.widget.AdapterView.OnItemClickListener;
import android.widget.ImageView.ScaleType;
import android.widget.ListView;

import com.rarnu.devlib.base.BaseFragment;
import com.rarnu.devlib.common.ISliding;
import com.rarnu.utils.UIUtils;
import com.zoe.calendar.CityActivity;
import com.zoe.calendar.FeedbackActivity;
import com.zoe.calendar.Global;
import com.zoe.calendar.R;
import com.zoe.calendar.SettingsActivity;
import com.zoe.calendar.adapter.LeftMenuAdapter;
import com.zoe.calendar.classes.LeftMenuItem;
import com.zoe.calendar.classes.MotionItem;
import com.zoe.calendar.database.QueryUtils;

public class LeftMenuFragment extends BaseFragment implements
		OnItemClickListener, OnTouchListener {

	TextView tvLeftTitle;
	ListView lvLeftCard;
	LeftMenuAdapter adapter;
	List<LeftMenuItem> list;

	LinearLayout layEat;
	ImageView[] ivEat;
	List<MotionItem> listMotion;
	TextView[] tvMotions;
	TextView tvFirstDay;
	RelativeLayout layMotions;

	int positionEater = 0;

	public LeftMenuFragment(String tag) {
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
		tvLeftTitle = (TextView) innerView.findViewById(R.id.tvLeftTitle);
		lvLeftCard = (ListView) innerView.findViewById(R.id.lvLeftCard);

		String[] titles = getResources()
				.getStringArray(R.array.title_left_menu);
		list = new ArrayList<LeftMenuItem>();

		for (int i = 0; i < titles.length; i++) {
			list.add(new LeftMenuItem(titles[i], getResources().getIdentifier(
					String.format("left_icon_%d", i + 1), "drawable",
					getActivity().getPackageName())));
		}

		adapter = new LeftMenuAdapter(getActivity(), list);
		lvLeftCard.setAdapter(adapter);

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
	public void onResume() {
		super.onResume();
		if (Global.city != null) {
			tvLeftTitle.setText(Global.city);
		}
		rebuildUI();
	}

	@Override
	public void initEvents() {
		lvLeftCard.setOnItemClickListener(this);
		layEat.setOnTouchListener(this);
	}

	private void rebuildUI() {
		try {
			QueryUtils.initMotion(getActivity());
		} catch (Exception e) {
		}
		try {
			listMotion = QueryUtils.queryMotion(getActivity());
			if (listMotion != null) {
				while (listMotion.size() < 10) {
					listMotion.add(new MotionItem());
				}
			}
		} catch (Exception e) {
		}
		buildMotionBoard();
		if (listMotion != null && listMotion.size() != 0) {
			if (listMotion.get(listMotion.size() - 1)._id == -1) {
				tvFirstDay.setText(R.string.motion_past);
			} else {
				tvFirstDay.setText(getString(R.string.motion_first_day,
						listMotion.get(listMotion.size() - 1).month + 1,
						listMotion.get(listMotion.size() - 1).day));
			}
		}
		startShiningThread();
	}

	private void buildMotionBoard() {
		// spacing 8dip
		int width = UIUtils.getWidth() - UIUtils.dipToPx(150)
				- UIUtils.dipToPx(8) - UIUtils.dipToPx(120);
		width /= 10;
		tvMotions = new TextView[10];
		// listMotion maybe null?
		layMotions.removeAllViews();
		if (listMotion != null && listMotion.size() != 0) {
			for (int i = 0; i < listMotion.size(); i++) {

				tvMotions[i] = new TextView(getActivity());
				RelativeLayout.LayoutParams lp = new RelativeLayout.LayoutParams(
						width, UIUtils.dipToPx(listMotion.get(i).motion * 8));
				lp.leftMargin = ((width + UIUtils.dipToPx(12)) * (9 - i));
				lp.addRule(RelativeLayout.ALIGN_PARENT_BOTTOM,
						RelativeLayout.TRUE);
				tvMotions[i].setLayoutParams(lp);
				tvMotions[i].setBackgroundColor(0xFF666666);
				layMotions.addView(tvMotions[i]);
			}
		}
		if (listMotion != null && listMotion.size() != 0) {
			positionEater = listMotion.get(0).motion;
			moveEater();
		}
	}

	@Override
	public void initLogic() {

	}

	@Override
	public int getFragmentLayoutResId() {
		return R.layout.fragment_leftmenu;
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
		tvLeftTitle.setText(Global.city);
	}

	@Override
	public Bundle getFragmentState() {
		return null;
	}

	@Override
	public void onItemClick(AdapterView<?> parent, View v, int position, long id) {
		switch (parent.getId()) {
		case R.id.lvLeftCard:
			switch (position) {
			case 0:
				break;
			case 1:
				startActivity(new Intent(getActivity(), CityActivity.class));
				break;
			case 2:
				startActivity(new Intent(getActivity(), SettingsActivity.class));
				break;
			case 3:
				startActivity(new Intent(getActivity(), FeedbackActivity.class));
				break;
			}
			break;
		}

		((ISliding) getActivity()).toggle();

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
		if (positionEater < 0) {
			positionEater = 0;
		}
		if (positionEater > 9) {
			positionEater = 9;
		}
		for (int i = 0; i < ivEat.length; i++) {
			ivEat[i].setImageResource((i == positionEater) ? R.drawable.motion
					: (i < positionEater ? R.drawable.bean_gray
							: R.drawable.bean));
		}
		try {
			QueryUtils.updateMotion(getActivity(), positionEater);
			RelativeLayout.LayoutParams rlp = (RelativeLayout.LayoutParams) tvMotions[0]
					.getLayoutParams();
			listMotion.get(0).motion = positionEater + 1;
			rlp.height = UIUtils.dipToPx(listMotion.get(0).motion * 8);
			tvMotions[0].setLayoutParams(rlp);
		} catch (Exception e) {

		}
	}

	private void startShiningThread() {
		final Handler hShining = new Handler() {
			@Override
			public void handleMessage(Message msg) {
				if (msg.what == 1) {
					// ------x---
					// 0123456789
					int idx = msg.arg1;
					if (idx == 0) {
						if (positionEater != 9) {
							ivEat[9].setImageResource(R.drawable.bean);
						}
						if (positionEater != 0) {
							ivEat[0].setImageResource(R.drawable.bean_light);
						}
					} else {
						if (idx != positionEater) {
							if (idx < positionEater) {
								ivEat[idx - 1]
										.setImageResource(R.drawable.bean_gray);
								ivEat[idx]
										.setImageResource(R.drawable.bean_light);
							} else {
								ivEat[idx]
										.setImageResource(R.drawable.bean_light);
								if ((idx - 1) != positionEater) {
									ivEat[idx - 1]
											.setImageResource(R.drawable.bean);
								}
							}
						} else {
							ivEat[idx - 1]
									.setImageResource(R.drawable.bean_gray);

						}
					}

				}
				super.handleMessage(msg);
			}
		};
		new Thread(new Runnable() {

			@Override
			public void run() {
				int shiningId = 0;
				Message msg = null;
				while (true) {
					try {
						Thread.sleep(200);
					} catch (Exception e) {

					}
					msg = hShining.obtainMessage(1, shiningId, 0);
					hShining.sendMessage(msg);

					shiningId++;
					if (shiningId > 9) {
						shiningId = 0;
					}
				}

			}
		}).start();
	}

}
