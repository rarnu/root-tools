package com.yugioh.android.fragments;

import android.os.Bundle;
import android.view.Menu;
import android.widget.TextView;
import com.rarnu.devlib.base.BaseFragment;
import com.yugioh.android.R;
import com.yugioh.android.classes.CardInfo;
import com.yugioh.android.common.Config;
import com.yugioh.android.utils.ResourceUtils;

public class CardInfoCardFragment extends BaseFragment {

    TextView tvInfo;
    CardInfo info;

    int fontSize = -1;

    public CardInfoCardFragment() {
        super();
        tagText = ResourceUtils.getString(R.string.tag_card_info);
        tabTitle = ResourceUtils.getString(R.string.page_cardinfo);
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
        tvInfo = (TextView) innerView.findViewById(R.id.tvInfo);
    }

    @Override
    public void initEvents() {

    }

    @Override
    public void initLogic() {
        info = (CardInfo) getActivity().getIntent().getSerializableExtra(
                "cardinfo");

        tvInfo.setText(buildCardInfo(info));

        fontSize = Config.cfgGetFontSize(getActivity());
        if (fontSize == -1) {
            fontSize = (int) tvInfo.getTextSize();
        }
        tvInfo.setTextSize(fontSize);
    }

    private String buildCardInfo(CardInfo info) {
        StringBuilder sbInfo = new StringBuilder();
        sbInfo.append(buildInfoLine(R.string.name, info.getSCCardName()));
        sbInfo.append(buildInfoLine(R.string.japan_name, info.getJPCardName()));
        sbInfo.append(buildInfoLine(R.string.english_name, info.getENCardName()));
        sbInfo.append(buildInfoLine(R.string.type, info.getSCCardType()));

        if (info.getSCCardType().contains(
                getResources().getString(R.string.monster))) {
            sbInfo.append(buildInfoLine(R.string.split, ""));
            sbInfo.append(buildInfoLine(R.string.attribute,
                    info.getSCCardAttribute()));
            sbInfo.append(buildInfoLine(
                    R.string.level,
                    String.format(
                            "%d %s",
                            info.getCardStarNum(),
                            info.getSCCardType().contains(
                                    getResources().getString(R.string.overlay)) ? getResources()
                                    .getString(R.string.lad) : "")));
            sbInfo.append(buildInfoLine(R.string.race, info.getSCCardRace()));
            sbInfo.append(buildInfoLine(R.string.attack, info.getCardAtk2()));
            sbInfo.append(buildInfoLine(R.string.defense, info.getCardDef2()));
        }
        sbInfo.append(buildInfoLine(R.string.split, ""));
        sbInfo.append(buildInfoLine(R.string.limit, info.getSCCardBan()));
        sbInfo.append(buildInfoLine(R.string.pack, info.getCardBagNum()));
        sbInfo.append(buildInfoLine(R.string.belongs, info.getCardCamp()));
        sbInfo.append(buildInfoLine(R.string.password, info.getCardPass()));
        sbInfo.append(buildInfoLine(R.string.rare, info.getSCCardRare()));
        sbInfo.append(buildInfoLine(R.string.split, ""));
        sbInfo.append(buildInfoLine(R.string.effect, info.getSCCardDepict()));

        return sbInfo.toString();
    }

    private String buildInfoLine(int nameRes, String info) {
        String ret = getResources().getString(nameRes);
        if (nameRes != R.string.split) {
            ret += ": " + info;
        }
        ret += "\n";
        return ret;
    }

    @Override
    public int getFragmentLayoutResId() {
        return R.layout.fragment_cardinfo_card;
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
            title = info.getSCCardName();
        }
        return title;
    }

    @Override
    public Bundle getFragmentState() {
        return null;
    }

}
