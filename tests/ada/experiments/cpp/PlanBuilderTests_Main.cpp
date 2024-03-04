#include "PlanBuilderTests.hpp"

#include <iostream>

int
main(int argc, char** argv)
{
    uxas::service::test::PlanBuilderTests tests;

    std::cout << "Running Process_Task_Assignment_Summary_Test" << std::endl;
    tests.Process_Task_Assignment_Summary_Test();

    std::cout << "Running Process_Task_Implementation_Response_Vehicle_Exists_WPList_Empty_Test" << std::endl;
    tests.Process_Task_Implementation_Response_Vehicle_Exists_WPList_Empty_Test();

    std::cout << "Running Process_Task_Implementation_Response_Vehicle_Exists_WPList_Empty_Test" << std::endl;
    tests.Process_Task_Implementation_Response_Vehicle_Exists_WPList_NotEmpty_Test();

    std::cout << "Running Process_Task_Implementation_Response_Vehicle_DoesNotExists_Test" << std::endl;
    tests.Process_Task_Implementation_Response_Vehicle_DoesNotExists_Test();

    std::cout << "Running Check_Next_Task_Implementation_Request_Test" << std::endl;
    tests.Check_Next_Task_Implementation_Request_Test();

    std::cout << "End to End Tests Complete" << std::endl;
};
