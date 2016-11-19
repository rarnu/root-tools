package com.rarnu.tools.neo.fragment;

import android.app.Activity;
import android.content.Intent;
import android.content.SharedPreferences;
import android.os.Bundle;
import android.os.Handler;
import android.os.Message;
import android.preference.PreferenceManager;
import android.view.Menu;
import android.view.MenuItem;
import android.view.View;
import android.widget.ImageView;
import android.widget.RelativeLayout;
import android.widget.TextView;
import android.widget.Toast;
import com.rarnu.tools.neo.R;
import com.rarnu.tools.neo.api.API;
import com.rarnu.tools.neo.base.BaseFragment;
import org.w3c.dom.Text;

import java.lang.reflect.Field;

/**
 * Created by rarnu on 11/19/16.
 */
public class FeedbackFragment extends BaseFragment implements View.OnClickListener {

    private MenuItem miSend;
    private TextView etNickname;
    private TextView etComment;
    private RelativeLayout ph1, ph2, ph3, ph4, ph5;
    private ImageView imgP1, imgP2, imgP3, imgP4, imgP5;
    private SharedPreferences sp;
    private String path1 = "", path2 = "", path3 = "", path4 = "", path5 = "";

    private static final String KEY_NICKNAME = "__nickname";

    @Override
    public int getBarTitle() {
        return R.string.about_feedback;
    }

    @Override
    public String getCustomTitle() {
        return null;
    }

    @Override
    public void initComponents() {
        etNickname = (TextView) innerView.findViewById(R.id.etNickname);
        etComment = (TextView) innerView.findViewById(R.id.etComment);
        ph1 = (RelativeLayout) innerView.findViewById(R.id.ph1);
        ph2 = (RelativeLayout) innerView.findViewById(R.id.ph2);
        ph3 = (RelativeLayout) innerView.findViewById(R.id.ph3);
        ph4 = (RelativeLayout) innerView.findViewById(R.id.ph4);
        ph5 = (RelativeLayout) innerView.findViewById(R.id.ph5);
        imgP1 = (ImageView) innerView.findViewById(R.id.imgP1);
        imgP2 = (ImageView) innerView.findViewById(R.id.imgP2);
        imgP3 = (ImageView) innerView.findViewById(R.id.imgP3);
        imgP4 = (ImageView) innerView.findViewById(R.id.imgP4);
        imgP5 = (ImageView) innerView.findViewById(R.id.imgP5);
        sp = PreferenceManager.getDefaultSharedPreferences(getContext());
    }

    @Override
    public void initEvents() {
        ph1.setOnClickListener(this);
        ph2.setOnClickListener(this);
        ph3.setOnClickListener(this);
        ph4.setOnClickListener(this);
        ph5.setOnClickListener(this);
    }

    @Override
    public void initLogic() {
        // load nickname cache
        etNickname.setText(sp.getString(KEY_NICKNAME, ""));
    }

    @Override
    public int getFragmentLayoutResId() {
        return R.layout.fragment_feedback;
    }

    @Override
    public String getMainActivityName() {
        return null;
    }

    @Override
    public void initMenu(Menu menu) {
        menu.clear();
        miSend = menu.add(0, 1, 1, R.string.ab_send);
        miSend.setIcon(android.R.drawable.ic_menu_send);
        miSend.setShowAsAction(MenuItem.SHOW_AS_ACTION_ALWAYS);
    }

    @Override
    public boolean onOptionsItemSelected(MenuItem item) {
        switch (item.getItemId()) {
            case 1:
                miSend.setEnabled(false);
                sendFeedback();
                break;
        }
        return true;
    }

    private Handler hFeedback = new Handler() {
        @Override
        public void handleMessage(Message msg) {
            miSend.setEnabled(true);
            if (msg.what == 0) {
                Toast.makeText(getContext(), R.string.toast_send_feedback_ok, Toast.LENGTH_SHORT).show();
                getActivity().finish();
            } else {
                Toast.makeText(getContext(), R.string.toast_send_feedback_fail, Toast.LENGTH_SHORT).show();
            }
            super.handleMessage(msg);
        }
    };

    private void sendFeedback() {

        final String nickname = etNickname.getText().toString();
        if (!nickname.equals("")) {
            Toast.makeText(getContext(), R.string.toast_nickname_empty, Toast.LENGTH_SHORT).show();
            return;
        }
        final String comment = etComment.getText().toString();
        sp.edit().putString(KEY_NICKNAME, etNickname.getText().toString()).apply();
        new Thread(new Runnable() {
            @Override
            public void run() {
                // send feedback async
                boolean ret = API.sendFeedback(nickname, comment, path1, path2, path3, path4, path5);
                hFeedback.sendEmptyMessage(ret ? 0 : 1);
            }
        }).start();
    }

    @Override
    public void onGetNewArguments(Bundle bn) {

    }

    @Override
    public Bundle getFragmentState() {
        return null;
    }

    @Override
    public void onClick(View v) {
        // TODO: upload screenshot

    }

    @Override
    public void onActivityResult(int requestCode, int resultCode, Intent data) {
        // TODO: album callback data
        if (resultCode != Activity.RESULT_OK) {
            return;
        }

        String vName = "imgP" + requestCode;
        Field fImg = getClass().getDeclaredField(vName);

    }
}
