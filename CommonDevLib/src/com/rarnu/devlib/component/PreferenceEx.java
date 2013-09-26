package com.rarnu.devlib.component;

import android.content.Context;
import android.preference.Preference;
import android.util.AttributeSet;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ImageView;
import android.widget.RelativeLayout;
import android.widget.TextView;
import com.rarnu.devlib.R;

public class PreferenceEx extends Preference {

    public static final int STATE_NORMAL = 0;
    public static final int STATE_WARNING = 1;
    public static final int STATE_BANNED = 2;
    RelativeLayout layPref;
    ImageView pref_icon;
    TextView pref_title;
    TextView pref_summary;
    ImageView pref_warning;
    int status;
    View innerView;

    public PreferenceEx(Context context, AttributeSet attrs, int defStyle) {
        super(context, attrs, defStyle);
    }

    public PreferenceEx(Context context, AttributeSet attrs) {
        super(context, attrs);
    }

    public PreferenceEx(Context context) {
        super(context);
    }

    public void setStatus(int state) {
        this.status = state;
    }

    public void resetStatus(int state) {
        this.status = state;
        switch (status) {
            case STATE_NORMAL:
                pref_warning.setImageDrawable(null);
                break;
            case STATE_WARNING:
                pref_warning.setImageResource(R.drawable.warning);
                break;
            case STATE_BANNED:
                pref_warning.setImageResource(R.drawable.banned);
                break;
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
        resetStatus(status);
    }

    @Override
    protected View onCreateView(ViewGroup parent) {
        if (innerView == null) {
            innerView = LayoutInflater.from(getContext()).inflate(R.layout.comp_preference, parent, false);
            layPref = (RelativeLayout) innerView.findViewById(R.id.layPref);
            pref_icon = (ImageView) innerView.findViewById(R.id.pref_icon);
            pref_title = (TextView) innerView.findViewById(R.id.pref_title);
            pref_summary = (TextView) innerView.findViewById(R.id.pref_summary);
            pref_warning = (ImageView) innerView.findViewById(R.id.pref_warning);
        }
        return innerView;
    }

    @Override
    public void setTitle(int titleResId) {
        super.setTitle(titleResId);
        if (pref_title != null) {
            pref_title.setText(titleResId);
        }
    }

    @Override
    public void setSummary(int summaryResId) {
        super.setSummary(summaryResId);
        if (pref_summary != null) {
            pref_summary.setText(getSummary());
            if (getSummary() == null || getSummary().equals("")) {
                pref_summary.setVisibility(View.GONE);
            }
        }
    }

    @Override
    public void setIcon(int iconResId) {
        super.setIcon(iconResId);
        if (pref_icon != null) {
            pref_icon.setImageDrawable(getIcon());
        }
    }
}
