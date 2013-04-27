package com.yugioh.android.fragments;

import java.util.Timer;
import java.util.TimerTask;

import android.os.Bundle;
import android.os.Handler;
import android.os.Message;
import android.view.Menu;
import android.view.View;
import android.view.View.OnClickListener;
import android.widget.ImageView;

import com.rarnu.devlib.base.BaseFragment;
import com.yugioh.android.R;

public class CoinDiceFragment extends BaseFragment implements OnClickListener {

	ImageView imgCoinDice;
	Timer tmrCloseWindow;
	int type = 0;
	int count = 0;

	@Override
	public int getBarTitle() {
		return R.string.page_tool;
	}

	@Override
	public int getBarTitleWithPath() {
		return R.string.page_tool;
	}

	@Override
	public String getCustomTitle() {
		return null;
	}

	@Override
	protected int getFragmentLayoutResId() {
		return R.layout.fragment_coindice;
	}

	@Override
	protected String getMainActivityName() {
		return "";
	}

	@Override
	protected void initComponents() {
		imgCoinDice = (ImageView) innerView.findViewById(R.id.imgCoinDice);

	}

	@Override
	protected void initEvents() {
		imgCoinDice.setOnClickListener(this);
	}

	final Handler h = new Handler() {

		@Override
		public void handleMessage(Message msg) {
			if (msg.what == 1) {
				doClose();
			}
			super.handleMessage(msg);
		}
	};

	@Override
	protected void initLogic() {
		type = getActivity().getIntent().getIntExtra("type", 0);
		if (type == 0) {
			getActivity().finish();
			return;
		}
		count = 0;
		if (type == 1) {
			doDice();
		} else if (type == 2) {
			doCoin();
		}

		tmrCloseWindow = new Timer();
		tmrCloseWindow.schedule(new TimerTask() {

			@Override
			public void run() {
				tmrCloseWindow.cancel();
				h.sendEmptyMessage(1);

			}
		}, 2000);

	}

	@Override
	public void onDestroy() {
		stopTimer();
		super.onDestroy();
	}

	@Override
	protected void initMenu(Menu menu) {

	}

	@Override
	protected void onGetNewArguments(Bundle bn) {

	}

	private void doDice() {
		// dice
		count++;
		int dice = (int) ((Math.random() * 6) + 1);
		switch (dice) {
		case 1:
			imgCoinDice.setImageDrawable(getResources().getDrawable(
					R.drawable.dice1));
			break;
		case 2:
			imgCoinDice.setImageDrawable(getResources().getDrawable(
					R.drawable.dice2));
			break;
		case 3:
			imgCoinDice.setImageDrawable(getResources().getDrawable(
					R.drawable.dice3));
			break;
		case 4:
			imgCoinDice.setImageDrawable(getResources().getDrawable(
					R.drawable.dice4));
			break;
		case 5:
			imgCoinDice.setImageDrawable(getResources().getDrawable(
					R.drawable.dice5));
			break;
		case 6:
			imgCoinDice.setImageDrawable(getResources().getDrawable(
					R.drawable.dice6));
			break;
		}

	}

	private void doCoin() {
		// coin
		count++;
		int coin = (int) ((Math.random() * 2) + 1);
		switch (coin) {
		case 1:
			imgCoinDice.setImageDrawable(getResources().getDrawable(
					R.drawable.coin1));
			break;
		case 2:
			imgCoinDice.setImageDrawable(getResources().getDrawable(
					R.drawable.coin2));
			break;
		}

	}

	@Override
	public void onClick(View v) {
		stopTimer();
		doClose();
	}

	private void stopTimer() {
		try {
			tmrCloseWindow.cancel();
		} catch (Exception e) {

		}
		tmrCloseWindow = null;
	}

	private void doClose() {
		try {
			getActivity().finish();
		} catch (Exception e) {

		}
	}

}
