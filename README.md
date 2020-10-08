# DpssModel

This branch is included in OpenUxAS for historical reasons. OpenUxAS includes 
an implementation of the Decentralized Perimeter Surveillance System (DPSS), a 
multi-UAV protocol for evenly dividing surveillance of a perimeter among N > 1 
UAVs in a decentralized way. A "paper-and-pencil" proof of the convergence 
time of DPSS was originally given in:

Kingston, Derek, Randal W. Beard, and Ryan S. Holt. 
"Decentralized perimeter surveillance using a team of UAVs." 
IEEE Transactions on Robotics 24.6 (2008): 1394-1404.
[Link](https://www.researchgate.net/profile/Randal_Beard2/publication/220397073_Decentralized_Perimeter_Surveillance_Using_a_Team_of_UAVs/links/556c8d6d08aeccd7773be43d/Decentralized-Perimeter-Surveillance-Using-a-Team-of-UAVs.pdf)

A formal methods tool, specifically the AGREE model checker, was used used to 
model this protocol and find an error in the original proof. The paper 
describing this result is:

Davis, Jennifer A., Laura R. Humphrey, and Derek B. Kingston. 
"When Human Intuition Fails: Using Formal Methods to Find an Error in the 
'Proof' of a Multi-agent Protocol." International Conference on Computer Aided 
Verification. Springer, Cham, 2019.
[Link](https://link.springer.com/content/pdf/10.1007%2F978-3-030-25540-4_20.pdf)

The models referenced in that paper are included in this branch.

Other models related to this work can be found on the dpssModel branch of the  
archived OpenUxAS "Summer of Innovation" repository:
https://github.com/afrl-rq/OpenUxAS-SoI/tree/dpssModel

