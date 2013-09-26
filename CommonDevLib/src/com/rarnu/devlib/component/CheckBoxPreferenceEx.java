package com.rarnu.devlib.component;

import android.content.Context;
import android.preference.CheckBoxPreference;
import android.util.AttributeSet;
import android.view.LayoutInflater;
import android.view.View;
import android.view.View.OnClickListener;
import android.view.ViewGroup;
import android.widget.CheckBox;
import android.widget.ImageView;
import android.widget.RelativeLayout;
import android.widget.TextView;
import com.rarnu.devlib.R;

public class CheckBoxPreferenceEx extends CheckBoxPreference {

    RelativeLayout layPref;
    ImageView pref_icon;
    TextView pref_title;
    TextView pref_summary;
    CheckBox pref_checkbox;
    View innerView;
    boolean stateChecked = false;
    OnClickListener checkboxClick;

    public CheckBoxPreferenceEx(Context context, AttributeSet attrs, int defStyle) {
        super(context, attrs, defStyle);
    }

    public CheckBoxPreferenceEx(Context context, AttributeSet attrs) {
        super(context, attrs);
    }

    public CheckBoxPreferenceEx(Context context) {
        super(context);
    }

    public boolean isStateChecked() {
        return stateChecked;
    }

    public void setStateChecked(boolean checked) {
        stateChecked = checked;
        if (pref_checkbox != null) {
            pref_checkbox.setChecked(checked);
        }
    }

    @Override
    protected void onBindView(View view) {
        super.onBindView(view);
        pref_title.setText(getTitle());
        pref_summary.setText(getSummary());
        if (getSummary() == null || getSummary().equals("")) {
            pref_summary.setVisibility(View.GONE);
        }
        pref_icon.setImageDrawable(getIcon());
        pref_checkbox.setChecked(stateChecked);
        pref_checkbox.setOnClickListener(checkboxClick);

    }

    @Override
    protected View onCreateView(ViewGroup parent) {
        if (innerView == null) {
            innerView = LayoutInflater.from(getContext()).inflate(R.layout.comp_preference_checkbox, parent, false);
            layPref = (RelativeLayout) innerView.findViewById(R.id.layPref);
            pref_icon = (ImageView) innerView.findViewById(R.id.pref_icon);
            pref_title = (TextView) innerView.findViewById(R.id.pref_title);
            pref_summary = (TextView) innerView.findViewById(R.id.pref_summary);
            pref_checkbox = (CheckBox) innerView.findViewById(R.id.pref_checkbox);
            pref_checkbox.setTag(getKey());

        }
        return innerView;
    }

    @Override
    public void setTitle(int titleResId) {
        super.setTitle(titleResId);
        pref_title.setText(titleResId);
    }

    public void setSummary(int summaryResId) {
        super.setSummary(summaryResId);
        pref_summary.setText(getSummary());
        if (getSummary() == null || getSummary().equals("")) {
            pref_summary.setVisibility(View.GONE);
        }
    }

    public void setOnCheckboxClickListener(OnClickListener checkboxClick) {
        this.checkboxClick = checkboxClick;
        if (pref_checkbox != null) {
            pref_checkbox.setOnClickListener(checkboxClick);
        }
    }
}
