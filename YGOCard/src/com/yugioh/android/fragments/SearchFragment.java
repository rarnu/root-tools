package com.yugioh.android.fragments;

import android.app.Activity;
import android.content.Intent;
import android.os.Bundle;
import android.os.Handler;
import android.os.Message;
import android.view.Menu;
import android.view.MenuItem;
import android.view.View;
import android.widget.AdapterView;
import android.widget.AdapterView.OnItemSelectedListener;
import android.widget.ArrayAdapter;
import android.widget.EditText;
import android.widget.Spinner;
import com.rarnu.devlib.base.BaseFragment;
import com.rarnu.utils.ResourceUtils;
import com.yugioh.android.R;
import com.yugioh.android.SearchResultActivity;
import com.yugioh.android.common.MenuIds;
import com.yugioh.android.define.CardConstDefine;

import java.util.List;

public class SearchFragment extends BaseFragment implements OnItemSelectedListener {

    Spinner spCardRace, spCardBelongs, spCardType, spCardAttribute, spCardLevel, spCardRare, spCardLimit, spCardTunner;
    EditText etCardName, etCardAttack, etCardDefense, etEffectText;

    MenuItem itemSearch;
    MenuItem itemReset;

    private BaseFragment searchResultFragment = null;

    public SearchFragment() {
        super();
        tabTitle = ResourceUtils.getString(R.string.page_search);
    }

    public void registerSearchResult(BaseFragment intf) {
        this.searchResultFragment = intf;
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
    public void initComponents() {
        etCardName = (EditText) innerView.findViewById(R.id.etCardName);
        etCardAttack = (EditText) innerView.findViewById(R.id.etCardAttack);
        etCardDefense = (EditText) innerView.findViewById(R.id.etCardDefense);
        etEffectText = (EditText) innerView.findViewById(R.id.etEffectText);
        spCardRace = (Spinner) innerView.findViewById(R.id.spCardRace);
        spCardBelongs = (Spinner) innerView.findViewById(R.id.spCardBelongs);
        spCardType = (Spinner) innerView.findViewById(R.id.spCardType);
        spCardAttribute = (Spinner) innerView.findViewById(R.id.spCardAttribute);
        spCardLevel = (Spinner) innerView.findViewById(R.id.spCardLevel);
        spCardRare = (Spinner) innerView.findViewById(R.id.spCardRare);
        spCardLimit = (Spinner) innerView.findViewById(R.id.spCardLimit);
        spCardTunner = (Spinner) innerView.findViewById(R.id.spCardTunner);

        etCardName.requestFocus();

    }

    @Override
    public void onResume() {
        super.onResume();
    }

    @Override
    public void initEvents() {
    }

    @Override
    public void initLogic() {
        setSpinner(spCardRace, CardConstDefine.DEFID_CARDRACE);
        setSpinner(spCardBelongs, CardConstDefine.DEFID_CARDBELONGS);
        setSpinner(spCardType, CardConstDefine.DEFID_CARDTYPE);
        setSpinner(spCardAttribute, CardConstDefine.DEFID_CARDATTRITUBE);
        setSpinner(spCardLevel, CardConstDefine.DEFID_CARDLEVEL);
        setSpinner(spCardRare, CardConstDefine.DEFID_CARDRARE);
        setSpinner(spCardLimit, CardConstDefine.DEFID_CARDLIMIT);
        setSpinner(spCardTunner, CardConstDefine.DEFID_CARDTUNNER);

    }

    private void setSpinner(final Spinner sp, final int type) {
        sp.setOnItemSelectedListener(this);

        final Handler hSpin = new Handler() {
            @Override
            public void handleMessage(Message msg) {
                if (msg.what == 1) {
                    @SuppressWarnings("unchecked")
                    List<String> list = (List<String>) msg.obj;
                    if (list != null) {
                        ArrayAdapter<String> adapter = new ArrayAdapter<String>(getActivity(), R.layout.item_spin, list);
                        sp.setAdapter(adapter);
                        sp.setSelection(0);
                    }
                }
                super.handleMessage(msg);
            }
        };

        new Thread(new Runnable() {

            @Override
            public void run() {
                List<String> list = null;
                switch (type) {
                    case CardConstDefine.DEFID_CARDRACE:
                        list = CardConstDefine.getCardRace();
                        list.add(0, getResources().getString(R.string.search_na));
                        break;
                    case CardConstDefine.DEFID_CARDBELONGS:
                        list = CardConstDefine.getCardBelongs();
                        break;
                    case CardConstDefine.DEFID_CARDTYPE:
                        list = CardConstDefine.getCardType();
                        list.add(0, getResources().getString(R.string.search_na));
                        break;

                    case CardConstDefine.DEFID_CARDATTRITUBE:
                        list = CardConstDefine.getCardAttribute();
                        list.add(0, getResources().getString(R.string.search_na));
                        break;

                    case CardConstDefine.DEFID_CARDLEVEL:
                        list = CardConstDefine.getCardLevel();
                        list.add(0, getResources().getString(R.string.search_na));
                        break;

                    case CardConstDefine.DEFID_CARDRARE:
                        list = CardConstDefine.getCardCare();
                        list.add(0, getResources().getString(R.string.search_na));
                        break;

                    case CardConstDefine.DEFID_CARDLIMIT:
                        list = CardConstDefine.getCardLimit();
                        list.add(0, getResources().getString(R.string.search_na));
                        break;
                    case CardConstDefine.DEFID_CARDTUNNER:
                        list = CardConstDefine.getCardTunner();
                        list.add(0, getResources().getString(R.string.search_na));
                        break;
                }

                Message msg = new Message();
                msg.what = 1;
                msg.obj = list;
                hSpin.sendMessage(msg);

            }
        }).start();

    }

    @Override
    public int getFragmentLayoutResId() {
        return R.layout.fragment_search;
    }

    @Override
    public String getMainActivityName() {
        return "";
    }

    @Override
    public void initMenu(Menu menu) {
        itemSearch = menu.add(0, MenuIds.MENUID_SEARCH, 98, R.string.search_search);
        itemSearch.setIcon(android.R.drawable.ic_menu_search);
        itemSearch.setShowAsAction(MenuItem.SHOW_AS_ACTION_ALWAYS);
        itemReset = menu.add(0, MenuIds.MENUID_RESET, 99, R.string.search_reset);
        itemReset.setIcon(android.R.drawable.ic_menu_close_clear_cancel);
        itemReset.setShowAsAction(MenuItem.SHOW_AS_ACTION_ALWAYS);
    }

    @Override
    public boolean onOptionsItemSelected(MenuItem item) {

        switch (item.getItemId()) {
            case MenuIds.MENUID_SEARCH:
                doSearchCard();
                break;
            case MenuIds.MENUID_RESET:
                doSearchReset();
                break;
        }

        return true;
    }

    private void doSearchCard() {
        String cardName = etCardName.getText().toString();
        String cardType = "";
        if (spCardType.getSelectedItemPosition() != 0) {
            cardType = (String) spCardType.getSelectedItem();
        }
        String cardAttribute = "";
        if (spCardAttribute.getSelectedItemPosition() != 0) {
            cardAttribute = (String) spCardAttribute.getSelectedItem();
        }
        int cardLevel = 0;
        if (spCardLevel.getSelectedItemPosition() != 0) {
            cardLevel = Integer.parseInt((String) spCardLevel.getSelectedItem());
        }
        String cardRare = "";
        if (spCardRare.getSelectedItemPosition() != 0) {
            cardRare = (String) spCardRare.getSelectedItem();
        }
        String cardRace = "";
        if (spCardRace.getSelectedItemPosition() != 0) {
            cardRace = (String) spCardRace.getSelectedItem();
        }
        String cardBelongs = "";
        if (spCardBelongs.getSelectedItemPosition() != 0) {
            cardBelongs = (String) spCardBelongs.getSelectedItem();
        }
        String cardAtk = etCardAttack.getText().toString();
        String cardDef = etCardDefense.getText().toString();

        String cardLimit = "";
        if (spCardLimit.getSelectedItemPosition() != 0) {
            cardLimit = (String) spCardLimit.getSelectedItem();
        }

        String cardTunner = (String) spCardTunner.getSelectedItem();
        if (cardTunner.equals(getString(R.string.search_na))) {
            cardTunner = "";
        }

        String cardEffectText = etEffectText.getText().toString();

        Bundle bn = new Bundle();
        bn.putString("cardType", cardType);
        bn.putString("cardAttribute", cardAttribute);
        bn.putInt("cardLevel", cardLevel);
        bn.putString("cardRace", cardRace);
        bn.putString("cardName", cardName);
        bn.putString("cardAtk", cardAtk);
        bn.putString("cardDef", cardDef);
        bn.putString("cardRare", cardRare);
        bn.putString("cardBelongs", cardBelongs);
        bn.putString("cardLimit", cardLimit);
        bn.putString("cardTunner", cardTunner);
        bn.putString("cardEffectText", cardEffectText);

        Intent inResult = new Intent(getActivity(), SearchResultActivity.class);
        inResult.putExtras(bn);
        startActivity(inResult);
    }

    private void doSearchReset() {
        spCardAttribute.setSelection(0);
        spCardBelongs.setSelection(0);
        spCardLevel.setSelection(0);
        spCardLimit.setSelection(0);
        spCardRace.setSelection(0);
        spCardRare.setSelection(0);
        spCardTunner.setSelection(0);
        spCardType.setSelection(0);
        etCardName.setText("");
        etCardAttack.setText("");
        etCardDefense.setText("");
        etEffectText.setText("");
    }

    @Override
    public void onGetNewArguments(Bundle bn) {
        if (bn.getString("data").equals("search")) {
            doSearchCard();
        } else if (bn.getString("data").equals("reset")) {
            doSearchReset();
        }
    }

    @Override
    public void onItemSelected(AdapterView<?> parent, View view, int position, long id) {
        switch (parent.getId()) {
            case R.id.spCardType:
                spCardTunner.setEnabled(position <= 7 && position > 0);
                break;
        }
    }

    @Override
    public void onNothingSelected(AdapterView<?> parent) {

    }

    @Override
    public String getCustomTitle() {
        return null;
    }

    @Override
    public Bundle getFragmentState() {
        return null;
    }

    @Override
    public void onActivityResult(int requestCode, int resultCode, Intent data) {
        if (resultCode != Activity.RESULT_OK) {
            return;
        }
        switch (requestCode) {
            case 0:
                etCardName.setText(data.getStringExtra("name"));
                break;
        }
    }
}
