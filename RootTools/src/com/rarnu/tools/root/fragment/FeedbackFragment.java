package com.rarnu.tools.root.fragment;

import android.os.Bundle;
import android.os.Handler;
import android.os.Message;
import android.view.Menu;
import android.view.MenuItem;
import android.view.View;
import android.widget.EditText;
import android.widget.Toast;
import com.rarnu.devlib.base.BaseFragment;
import com.rarnu.devlib.component.DataProgressBar;
import com.rarnu.tools.root.GlobalInstance;
import com.rarnu.tools.root.MainActivity;
import com.rarnu.tools.root.R;
import com.rarnu.tools.root.api.MobileApi;
import com.rarnu.tools.root.common.MenuItemIds;
import com.rarnu.tools.root.utils.DeviceUtils;

public class FeedbackFragment extends BaseFragment {

    final Handler hSendFeedback = new Handler() {
        @Override
        public void handleMessage(Message msg) {
            if (msg.what == 1) {
                if (msg.arg1 != 0) {
                    Toast.makeText(getActivity(), R.string.send_feedback_succ, Toast.LENGTH_LONG).show();
                    etFeedback.setText("");
                } else {
                    Toast.makeText(getActivity(), R.string.send_feedback_fail, Toast.LENGTH_LONG).show();
                }
                progressFeedback.setVisibility(View.GONE);
                itemSend.setEnabled(true);
                etFeedback.setEnabled(true);
            }
            super.handleMessage(msg);
        }

    };
    EditText etFeedback;
    DataProgressBar progressFeedback;
    MenuItem itemSend;

    @Override
    public boolean onOptionsItemSelected(MenuItem item) {
        if (item.getItemId() == MenuItemIds.MENU_SEND) {
            String comment = etFeedback.getText().toString();
            if (comment.equals("")) {
                Toast.makeText(getActivity(), R.string.empty_feedback, Toast.LENGTH_LONG).show();
                return true;
            }
            doSendFeedbackT(comment);
        }
        return true;
    }

    private void doSendFeedbackT(final String comment) {
        progressFeedback.setAppName(getString(R.string.sending));
        progressFeedback.setVisibility(View.VISIBLE);
        etFeedback.setEnabled(false);
        itemSend.setEnabled(false);

        new Thread(new Runnable() {
            @Override
            public void run() {
                boolean ret = MobileApi.userFeedback(DeviceUtils.getDeviceUniqueId(getActivity()), GlobalInstance.device.roProductModel, GlobalInstance.device.roBuildVersionSdk, "", GlobalInstance.device.roBuildDescription, comment);
                Message msg = new Message();
                msg.what = 1;
                msg.arg1 = (ret ? 1 : 0);
                hSendFeedback.sendMessage(msg);
            }
        }).start();
    }

    @Override
    public int getBarTitle() {
        return R.string.feedback;
    }

    @Override
    public int getBarTitleWithPath() {
        return R.string.feedback_with_path;
    }

    @Override
    public void initComponents() {
        etFeedback = (EditText) innerView.findViewById(R.id.etFeedback);
        progressFeedback = (DataProgressBar) innerView.findViewById(R.id.progressFeedback);

    }

    @Override
    public int getFragmentLayoutResId() {
        return R.layout.layout_user_feedback;
    }

    @Override
    public void initMenu(Menu menu) {
        itemSend = menu.add(0, MenuItemIds.MENU_SEND, 99, R.string.send);
        itemSend.setShowAsAction(MenuItem.SHOW_AS_ACTION_IF_ROOM);
        itemSend.setIcon(android.R.drawable.ic_menu_send);

    }

    @Override
    public void initLogic() {

    }

    @Override
    public void initEvents() {

    }

    @Override
    public String getMainActivityName() {
        return MainActivity.class.getName();
    }

    @Override
    public void onGetNewArguments(Bundle bn) {

    }

    @Override
    public String getCustomTitle() {
        return null;
    }

    @Override
    public Bundle getFragmentState() {
        return null;
    }

}
