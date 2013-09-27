package com.yugioh.android.fragments;

import android.os.Bundle;
import android.os.Handler;
import android.os.Message;
import android.view.Menu;
import android.view.View;
import android.view.View.OnClickListener;
import android.widget.ImageView;
import com.rarnu.devlib.base.BaseFragment;
import com.yugioh.android.R;

import java.util.Timer;
import java.util.TimerTask;

public class CoinDiceFragment extends BaseFragment implements OnClickListener {

    final Handler h = new Handler() {

        @Override
        public void handleMessage(Message msg) {
            if (msg.what == 1) {
                doClose();
            }
            super.handleMessage(msg);
        }
    };
    ImageView imgCoinDice;
    Timer tmrCloseWindow;
    int type = 0;
    int count = 0;
    int[] _dice = new int[]{R.drawable.dice1, R.drawable.dice2, R.drawable.dice3,
            R.drawable.dice4, R.drawable.dice5, R.drawable.dice6};
    int[] _coin = new int[]{R.drawable.coin1, R.drawable.coin2};

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
    public int getFragmentLayoutResId() {
        return R.layout.fragment_coindice;
    }

    @Override
    public String getMainActivityName() {
        return "";
    }

    @Override
    public void initComponents() {
        imgCoinDice = (ImageView) innerView.findViewById(R.id.imgCoinDice);

    }

    @Override
    public void initEvents() {
        imgCoinDice.setOnClickListener(this);
    }

    @Override
    public void initLogic() {
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
    public void initMenu(Menu menu) {

    }

    @Override
    public void onGetNewArguments(Bundle bn) {

    }

    private void doDice() {
        // dice
        count++;
        int dice = (int) ((Math.random() * 6) + 1);
        imgCoinDice.setImageResource(_dice[dice - 1]);
    }

    private void doCoin() {
        // coin
        count++;
        int coin = (int) ((Math.random() * 2) + 1);
        imgCoinDice.setImageResource(_coin[coin - 1]);
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

    @Override
    public Bundle getFragmentState() {
        return null;
    }

}
