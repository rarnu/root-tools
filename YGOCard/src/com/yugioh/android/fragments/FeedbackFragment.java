package com.yugioh.android.fragments;

import android.content.Loader;
import android.os.Bundle;
import android.view.Menu;
import android.view.MenuItem;
import android.view.View;
import android.widget.EditText;
import android.widget.TextView;
import android.widget.Toast;
import com.rarnu.devlib.base.BaseFragment;
import com.yugioh.android.R;
import com.yugioh.android.common.MenuIds;
import com.yugioh.android.loader.FeedbackSender;

public class FeedbackFragment extends BaseFragment implements Loader.OnLoadCompleteListener<Boolean> {

    MenuItem itemSend;
    EditText etFeedback;
    FeedbackSender sender;
    TextView tvSending;

    @Override
    public int getBarTitle() {
        return 0;
    }

    @Override
    public int getBarTitleWithPath() {
        return 0;
    }

    @Override
    public String getCustomTitle() {
        return null;
    }

    @Override
    public void initComponents() {
        etFeedback = (EditText) innerView.findViewById(R.id.etFeedback);
        sender = new FeedbackSender(getActivity());
        tvSending = (TextView) innerView.findViewById(R.id.tvSending);
    }

    @Override
    public void initEvents() {
        sender.registerListener(0, this);
    }

    @Override
    public void initLogic() {
    }

    @Override
    public int getFragmentLayoutResId() {
        return R.layout.fragment_feedback;
    }

    @Override
    public String getMainActivityName() {
        return "";
    }

    @Override
    public void initMenu(Menu menu) {
        itemSend = menu.add(0, MenuIds.MENUID_SEND, 99, R.string.send);
        itemSend.setIcon(android.R.drawable.ic_menu_send);
        itemSend.setShowAsAction(MenuItem.SHOW_AS_ACTION_IF_ROOM);
    }

    @Override
    public boolean onOptionsItemSelected(MenuItem item) {
        switch (item.getItemId()) {
            case MenuIds.MENUID_SEND:
                String text = etFeedback.getText().toString();
                if (text.equals("")) {
                    Toast.makeText(getActivity(), R.string.empty_feedback, Toast.LENGTH_LONG).show();
                    return true;
                }
                itemSend.setEnabled(false);
                etFeedback.setEnabled(false);
                tvSending.setVisibility(View.VISIBLE);
                sender.setText(text);
                sender.startLoading();
                break;
        }
        return true;
    }

    @Override
    public void onGetNewArguments(Bundle bn) {
    }

    @Override
    public Bundle getFragmentState() {
        return null;
    }

    @Override
    public void onLoadComplete(Loader<Boolean> loader, Boolean data) {
        boolean succ = false;
        if (data != null) {
            succ = data;
        }
        if (getActivity() != null) {
            tvSending.setVisibility(View.GONE);
            itemSend.setEnabled(true);
            etFeedback.setEnabled(true);
            Toast.makeText(getActivity(), succ ? R.string.feedback_send_ok : R.string.feedback_send_fail, Toast.LENGTH_LONG).show();
            if (succ) {
                getActivity().finish();
            }
        }
    }
}
