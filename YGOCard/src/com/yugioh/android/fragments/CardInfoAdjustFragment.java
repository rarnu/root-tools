package com.yugioh.android.fragments;

import android.os.Bundle;
import android.view.Menu;
import android.view.View;
import android.widget.TextView;
import com.rarnu.devlib.base.BaseFragment;
import com.rarnu.utils.ResourceUtils;
import com.yugioh.android.R;
import com.yugioh.android.classes.CardInfo;
import com.yugioh.android.common.Config;

public class CardInfoAdjustFragment extends BaseFragment {

    CardInfo info;
    TextView tvAdjust;
    TextView tvNoAdjust;
    int fontSize = -1;

    public CardInfoAdjustFragment() {
        super();
        tabTitle = ResourceUtils.getString(R.string.page_cardadjust);

    }

    @Override
    public int getBarTitle() {
        return 0;
    }

    @Override
    public int getBarTitleWithPath() {
        return 0;
    }

    @Override
    public void initComponents() {
        tvAdjust = (TextView) innerView.findViewById(R.id.tvAdjust);
        tvNoAdjust = (TextView) innerView.findViewById(R.id.tvNoAdjust);
    }

    @Override
    public void initEvents() {

    }

    @Override
    public void initLogic() {

        info = (CardInfo) getActivity().getIntent().getSerializableExtra("cardinfo");

        tvAdjust.setText(info.getAdjust());
        tvNoAdjust.setVisibility((info.getAdjust() == null || info.getAdjust().trim().equals("")) ? View.VISIBLE : View.GONE);

        fontSize = Config.cfgGetFontSize(getActivity());
        if (fontSize == -1) {
            fontSize = (int) tvAdjust.getTextSize();
        }
        tvAdjust.setTextSize(fontSize);
    }

    @Override
    public int getFragmentLayoutResId() {
        return R.layout.fragment_cardinfo_adjust;
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
    public String getCustomTitle() {
        String title = null;
        if (info != null) {
            title = info.getName();
        }
        return title;
    }

    @Override
    public Bundle getFragmentState() {
        return null;
    }

}
