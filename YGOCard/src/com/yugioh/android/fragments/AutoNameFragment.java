package com.yugioh.android.fragments;

import android.app.Activity;
import android.content.Intent;
import android.content.Loader;
import android.database.Cursor;
import android.os.Bundle;
import android.text.Editable;
import android.text.TextWatcher;
import android.view.Menu;
import android.view.View;
import android.widget.*;
import com.rarnu.devlib.base.BaseFragment;
import com.yugioh.android.R;
import com.yugioh.android.loader.AutoNameLoader;

public class AutoNameFragment extends BaseFragment implements AdapterView.OnItemClickListener, View.OnClickListener, Loader.OnLoadCompleteListener<Cursor> {

    EditText etCardName;
    ImageView btnSearch;
    ListView lvHint;
    Cursor cSearchResult;
    SimpleCursorAdapter adapterSearchResult;
    AutoNameLoader loader;

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
        etCardName = (EditText) innerView.findViewById(R.id.etCardName);
        btnSearch = (ImageView) innerView.findViewById(R.id.btnSearch);
        lvHint = (ListView) innerView.findViewById(R.id.lvHint);
        loader = new AutoNameLoader(getActivity());
    }

    @Override
    public void initEvents() {
        etCardName.addTextChangedListener(new TextWatcher() {
            @Override
            public void afterTextChanged(Editable s) {
                doSearchHint();
            }

            @Override
            public void beforeTextChanged(CharSequence s, int start, int count, int after) {

            }

            @Override
            public void onTextChanged(CharSequence s, int start, int before, int count) {

            }
        });
        lvHint.setOnItemClickListener(this);
        btnSearch.setOnClickListener(this);
        loader.registerListener(0, this);
    }

    @Override
    public void initLogic() {
        etCardName.requestFocus();
        String name = getArguments().getString("name");
        etCardName.setText(name);
        if (!name.equals("")) {
            doSearchHint();
            etCardName.setSelection(etCardName.getText().toString().length());
        }
    }

    private void doSearchHint() {
        loader.setName(etCardName.getText().toString());
        loader.startLoading();
    }

    @Override
    public int getFragmentLayoutResId() {
        return R.layout.fragment_auto_name;
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

    @Override
    public void onItemClick(AdapterView<?> parent, View view, int position, long id) {
        cSearchResult.moveToPosition(position);
        etCardName.setText(cSearchResult.getString(cSearchResult.getColumnIndex("name")));
        etCardName.setSelection(etCardName.getText().toString().length());
    }

    @Override
    public void onClick(View v) {
        switch (v.getId()) {
            case R.id.btnSearch:
                Intent inRet = new Intent();
                inRet.putExtra("name", etCardName.getText().toString());
                getActivity().setResult(Activity.RESULT_OK, inRet);
                getActivity().finish();
                break;
        }

    }

    @Override
    public void onLoadComplete(Loader<Cursor> loader, Cursor data) {
        if (data != null) {
            cSearchResult = data;
            adapterSearchResult = new SimpleCursorAdapter(getActivity(), R.layout.item_card, cSearchResult, new String[]{"name"}, new int[]{R.id.tvCardName}, CursorAdapter.FLAG_REGISTER_CONTENT_OBSERVER);
        } else {
            adapterSearchResult = null;
        }

        if (getActivity() != null) {
            lvHint.setAdapter(adapterSearchResult);
        }
    }
}
