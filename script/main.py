from CalredieAutomator import CalredieAutomator

if __name__ == '__main__':
    automator = CalredieAutomator()

    automator.load()
    automator.login()
    automator.extracts()
    automator.click_through_systems_tab_exports()
    automator.extracts()
    automator.click_through_udf_data_exports()

    automator.run_R_script()

    automator.close()
